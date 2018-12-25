{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE ExplicitForAll #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE FlexibleContexts #-}

module ElispParse.MacroExpansion (
    Macro(..)
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
import Control.Lens
import qualified GHC.Generics as G
import Data.Generics.Product
import Data.Generics.Sum
import qualified Data.Text.Lazy as T

import ElispParse.Common

data Macro = Macro
  { name :: Identifier
  , params :: [MacroParameter]
  , result :: InfiniteAST
  } deriving (Show, G.Generic)

data MacroParameter = RegularMP Identifier
                    | OptionalMP Identifier
                    | RestMP Identifier
  deriving (Show, G.Generic)

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
          Just $ length target == 1 + length (params macro)

        optionalFlag = FASTIdentifier_ "&optional"
        restFlag = FASTIdentifier_ "&rest"

        filterHeadEqual comp xs =
          bool xs (drop 1 xs)
          . isJust
          . preview (_head . only (comp)) $ xs

        outputAST = do
          target <- possiblyTarget
          macro <- macroCalled
          let targetParams = drop 1 target
              (regular, (optionals, rest)) =
                span (/= optionalFlag) targetParams
                & second ((span (/= restFlag)) . filterHeadEqual optionalFlag
                         >>> second (preview $ _tail . _head))


                -- & second (second (preview $ _tail . _head)
                --           . (span (/= restFlag)) . filterHeadEqual optionalFlag)
                  -- & _1 %~ (traverse (preview _FASTIdentifier))
          --     substitutions = M.fromList $ zip (params macro) (drop 1 target)
          --     applySub query = fromMaybe query $
          --       query ^? _FASTIdentifier >>= \i -> substitutions ^. at i
          -- Just $ transform applySub (result macro)
          undefined

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
  -- macroParams <- form ^? ix 2 . _FASTList
  --                >>= traverse (preview _FASTIdentifier)
  macroParams <- undefined
  macroBody <- form ^? ix 3
  pure $ Macro macroName macroParams macroBody
  --TODO: handle alised defmacro
  -- note that this will require more complex checking logic
  -- this is why we're not using pattern matching, if you're wondering
