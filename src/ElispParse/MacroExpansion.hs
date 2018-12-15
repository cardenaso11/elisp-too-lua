{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE TypeApplications#-}
{-# LANGUAGE ExplicitForAll #-}

module ElispParse.MacroExpansion (
    Macro(..)
  , macroExpand
  , macroExpandWith
) where

import Data.List (find)
import Data.Maybe
import Control.Lens
import GHC.Generics
import Data.Generics.Product
import Data.Generics.Sum

import qualified Data.Text as T

import ElispParse.Common

data Macro = Macro { name :: Identifier, params :: [Identifier], result :: InfiniteAST } deriving (Show, Generic)

macroExpandWith :: [Macro] -> InfiniteAST -> InfiniteAST
macroExpandWith macros expr@(FASTList (x:xs))
    | isJust (toMacro expr) = expr
    | FASTIdentifier ident <- x, Just macro <- find ((==ident).name) macros =
        subst macro xs
    | otherwise =
        FASTList (macroExpandWith macros <$> (x:xs))
macroExpandWith macros (Fix expr) = Fix (macroExpandWith macros <$> expr)

subst :: Macro -> [InfiniteAST] -> InfiniteAST
subst macro args =
    let argMap = zip (params macro) args
    in  go argMap (result macro)
    where
        go argMap i@(FASTIdentifier ident) = fromMaybe i $ lookup ident argMap
        go argMap (Fix expr) = Fix (go argMap <$> expr)

-- | Find macro definitions in the AST and expand their use sites.
macroExpand :: InfiniteAST -> InfiniteAST
macroExpand ex =
    let seed = (ex, expandOnce ex)
        step (a, b) = (b, expandOnce b)
    in  fst (until (uncurry (==)) step seed)

expandOnce :: InfiniteAST -> InfiniteAST
expandOnce (FASTList exprs) =
    let macros = mapMaybe toMacro exprs
    in  FASTList (expandOnce . macroExpandWith macros <$> exprs)
expandOnce expr
    | isMacro expr = expr
    | otherwise = Fix (expandOnce <$> unFix expr)
    where isMacro = isJust . toMacro

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