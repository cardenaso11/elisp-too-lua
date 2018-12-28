{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE ExplicitForAll #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TupleSections #-}

module ElispParse.MacroExpansion
  (
    Macro(..)
  , toMacro
  , macroExpand
  , macroExpandOnce
  , macroExpandWith
  , macroExpandOnceWith
  ) where

import Data.List (find)
import qualified Data.Map as M
import Data.Bool
import Data.Maybe
import Data.Bifunctor
import Control.Arrow ((>>>))
import Control.Monad
import Control.Lens
import qualified GHC.Generics as G
import Data.Generics.Product
import Data.Generics.Sum
import qualified Data.Text.Lazy as T

import ElispParse.CommonInternal

data Macro = Macro
  { name :: Identifier
  , regularParams :: [Identifier]
  , optionalParams :: [Identifier]
  , restParam :: Maybe Identifier
  , result :: InfiniteAST
  } deriving (Show, Eq, G.Generic)

optionalFlag = Identifier "&optional"
restFlag = Identifier "&rest"
nil = Identifier "nil"

untilStable :: forall a. Eq a => (a -> a) -> a -> a
untilStable f x = fst $ until (uncurry (==)) step seed
  where seed = (x, f x)
        step (_, b) = (b, f b)

macroExpandWith :: [Macro] -> InfiniteAST -> Maybe InfiniteAST
macroExpandWith macros = untilStable (macroExpandOnceWith macros =<<) . Just

-- expand macros once
macroExpandOnceWith :: [Macro] -> InfiniteAST -> Maybe InfiniteAST
macroExpandOnceWith macros = transformMOn ignoringMacros subst
  --NOTE: this probably isnt a legal traversal: look into it
  where
    ignoringMacros = plate . filtered (isNothing . toMacro)
    subst inputAST =
      if | isNothing macroCalled -> Just inputAST
         | isCorrectArity -> outputAST
         | otherwise -> Nothing
      where
        possiblyTarget = inputAST ^? _FASTList
        macroCalled = possiblyTarget ^? _Just . ix 0 . _FASTIdentifier >>=
          \x -> find ((x ==) . name) macros
        isCorrectArity = fromMaybe False $ do
          target <- possiblyTarget
          macro <- macroCalled
          let targetLen = length target
              regularParamLen = length (regularParams macro)
              optionalParamLen = length (optionalParams macro)
          Just $ isJust (restParam macro)
            || (length target >= 1 + regularParamLen
                && length target <=
                 1 + regularParamLen
                 + optionalParamLen + (fromEnum . isJust . restParam $ macro))

        outputAST = do
          target <- possiblyTarget
          macro <- macroCalled
          let targetTail = drop 1 target
              zippedRegular =
                zip (regularParams macro) targetTail
              zippedOptional =
                zip (optionalParams macro)
                (drop (length zippedRegular) targetTail
                 ++ repeat (FASTIdentifier nil))
              rest =
                toListOf traverse $ restParam macro
                <&> (, FASTList $
                       drop (length zippedRegular + length zippedOptional)
                       targetTail)
              substitutions =
                M.fromList $ zippedRegular ++ zippedOptional ++ rest
              applySub query = fromMaybe query $
                (query ^? _FASTIdentifier) >>= \i -> substitutions ^. at i
          Just $ transform applySub (result macro)

macroExpand :: InfiniteAST -> Maybe InfiniteAST
macroExpand inputAST = untilStable (macroExpandOnce =<<) $ Just inputAST

macroExpandOnce :: InfiniteAST -> Maybe InfiniteAST
macroExpandOnce inputAST = macroExpandOnceWith macros inputAST
  where macros = inputAST ^.. plate . (to toMacro . _Just)

toMacro :: InfiniteAST -> Maybe Macro
toMacro x = do
  form <- x ^? _FASTList
  form ^? ix 0 . _FASTIdentifier_ . only "defmacro"
  macroName <- form ^? ix 1 . _FASTIdentifier
  macroParams <- form ^? ix 2 . _FASTList
                 >>= traverse (preview _FASTIdentifier)
                 & mfilter (\xs -> occurrences optionalFlag xs <= 1
                                   && occurrences restFlag xs <= 1)
  let (regular, (optionals, rest)) =
        break (`elem` [optionalFlag, restFlag]) macroParams
        & second (span (/= restFlag) . filterHeadEqual optionalFlag
                   >>> second (preview $ _tail . _head))
  macroBody <- form ^? ix 3
  pure $ Macro macroName regular optionals rest macroBody
  where
    occurrences x = length . filter (==x)
    filterHeadEqual comp xs =
        bool xs (drop 1 xs)
        . isJust
        . preview (_head . only comp) $ xs

  --TODO: handle alised defmacro
  -- note that this will require more complex checking logic
  -- this is why we're not using pattern matching, if you're wondering
