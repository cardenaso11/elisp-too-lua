{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE TypeApplications#-}
{-# LANGUAGE ExplicitForAll #-}
{-# LANGUAGE GADTs #-}

module ElispParse.MacroExpansion (
    Macro(..)
  , macroExpand
  , macroExpandOnce
  , macroExpandWith
  , macroExpandOnceWith
) where

import Data.List (find)
import Data.Foldable
import qualified Data.List.NonEmpty as NE
import qualified Data.Map as M
import Data.Maybe
import Data.Bool
import Data.Data.Lens
import Control.Monad
import Control.Lens
import qualified GHC.Generics as G
import Data.Generics.Product
import Data.Generics.Sum

import qualified Data.Text.Lazy as T

import ElispParse.Common

import Debug.Trace

data Macro = Macro
  { name :: Identifier
  , params :: [Identifier]
  , result :: InfiniteAST
  } deriving (Show, G.Generic)

untilStable :: forall a. Eq a => (a -> a) -> a -> a
untilStable f x =
    let seed = (x, f x)
        step (_, b) = (b, f b)
    in  fst $ until (uncurry (==)) step seed

macroExpandWith :: [Macro] -> InfiniteAST -> Maybe InfiniteAST
macroExpandWith macros inputAST =
  untilStable (macroExpandOnceWith macros =<< ) $ Just inputAST

-- expand macros once
macroExpandOnceWith :: [Macro] -> InfiniteAST -> Maybe InfiniteAST
macroExpandOnceWith macros inputAST =
  let ignoringMacros' = plate . filtered (isNothing . toMacro)
      -- TODO: i think this isnt a legal traversal,,, if possible, try to make one
  in  foldrM (\m -> transformMOn ignoringMacros' (subst m)) inputAST macros

subst :: Macro -> InfiniteAST -> Maybe InfiniteAST
subst macro inputAST = --trace ("-- SUBST CALLED WITH: macro=" ++ show macro ++ ",inputAST=" ++ show inputAST ++ ",outputAST=" ++ show outputAST ++ " --") $
  if isNothing macroCalled
  then Just inputAST
  else
    if isCorrectArity
    then outputAST
    else Nothing
  where
    possiblyTarget = inputAST ^? _FASTList
    macroCalled = possiblyTarget >>=
      (\target -> target ^? ix 0 & mfilter (== FASTIdentifier (name macro)))
         --listToMaybe target == Just (FASTIdentifier $ name macro))
    isCorrectArity = maybe False
      (\target -> length target == 1 + length (params $ macro))
      possiblyTarget

    outputAST = possiblyTarget <&> \target ->
        let substitutions = M.fromList $ zip (params macro) (drop 1 target)
            applySub query = fromMaybe query $
              query ^? _FASTIdentifier >>= \i -> substitutions ^. at i
            -- applySub' :: InfiniteAST -> Maybe InfiniteAST
            -- applySub' query = maybe (Just query) (\i -> substitutions ^. at i)
            --   (query ^? _FASTIdentifier)
        in  transform applySub (result macro)

      -- & mfilter $ \thing -> undefined

      --(\target -> length target == 1 + length (params $ macro)
      --            && listToMaybe target == Just (FASTIdentifier $ name macro))

-- | Find top-level macro definitions in the AST and expand their use sites.
macroExpand :: InfiniteAST -> Maybe InfiniteAST
macroExpand inputAST = untilStable (macroExpandOnce =<<) $ Just inputAST

macroExpandOnce :: InfiniteAST -> Maybe InfiniteAST
macroExpandOnce inputAST =
  let macros = inputAST ^.. plate . (to toMacro . _Just)
  in  macroExpandOnceWith macros inputAST

toMacro :: InfiniteAST -> Maybe Macro
toMacro x = do
  form <- x ^? _Wrapped . _Ctor @"ASTList"
  form ^? ix 0 . _Wrapped . _Ctor @"ASTIdentifier" . only (Identifier "defmacro")
  macroName <- form ^? ix 1 . _Wrapped . _Ctor @"ASTIdentifier"
  macroParams <- form ^? ix 2 . _Wrapped . _Ctor @"ASTList"
    >>= traverse toIdentifier
  macroBody <- form ^? ix 3

  --FIXME: traverse to error on non identifiers is incorrect because of &rest

  --TODO: handle alised defmacro
  -- note that this will require more complex checking logic
  -- this is why we're not using pattern matching, if you're wondering

  pure $ Macro macroName macroParams macroBody

toIdentifier :: InfiniteAST -> Maybe Identifier
toIdentifier x = x ^? _Wrapped._Ctor @"ASTIdentifier"