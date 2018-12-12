{-# LANGUAGE OverloadedStrings #-}

module ElispParse.MacroExpansion (
    Macro(..)
  , macroExpand
  , macroExpandWith
) where

import Data.List (find)
import Data.Maybe
import qualified Data.Text as T

import ElispParse.Common

data Macro = Macro { name :: Identifier, params :: [Identifier], result :: InfiniteAST }

macroExpandWith :: [Macro] -> InfiniteAST -> InfiniteAST
macroExpandWith macros (Fix (ASTList (x:xs)))
    | Fix (ASTIdentifier ident) <- x, Just macro <- find ((==ident).name) macros = 
        subst macro xs
    | otherwise = 
        Fix (ASTList (macroExpandWith macros <$> (x:xs)))
macroExpandWith macros (Fix expr) = Fix (macroExpandWith macros <$> expr)

subst :: Macro -> [InfiniteAST] -> InfiniteAST
subst macro args =
    let argMap = zip (params macro) args
    in  go argMap (result macro)
    where
        go argMap i@(Fix (ASTIdentifier ident)) =
            case lookup ident argMap of
                Just arg -> arg
                Nothing -> i
        go argMap (Fix expr) = Fix (go argMap <$> expr)

-- | Find macro definitions in the AST and expand their use sites.
macroExpand :: InfiniteAST -> InfiniteAST
macroExpand (Fix (ASTList exprs)) =
    let macros = mapMaybe toMacro exprs
    in  Fix (ASTList (macroExpandWith macros <$> exprs))
macroExpand (Fix expr) = Fix (macroExpand <$> expr)

toMacro :: InfiniteAST -> Maybe Macro
toMacro (Fix (ASTList [Fix (ASTIdentifier (Identifier "defmacro")), Fix (ASTIdentifier macroName), Fix (ASTList macroParams), macroBody])) = do
    macroParams' <- traverse toIdentifier macroParams
    Just (Macro macroName macroParams' macroBody)
toMacro _ = Nothing

toIdentifier :: InfiniteAST -> Maybe Identifier
toIdentifier (Fix (ASTIdentifier i)) = Just i
toIdentifier _ = Nothing