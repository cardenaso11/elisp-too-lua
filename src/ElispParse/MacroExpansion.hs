module ElispParse.MacroExpansion (
    Macro(..)
  , macroExpand
) where

import qualified Data.Text as T

import ElispParse.Common

data Macro a = Macro { name :: Identifier, params :: [Identifier], result :: InfiniteAST }

macroExpand :: Macro a -> InfiniteAST -> InfiniteAST
macroExpand macro (Fix expr@(ASTList (x:xs)))
    | Fix (ASTIdentifier ident) <- x, ident == name macro = 
        subst macro xs
    | otherwise = 
        Fix (ASTList (macroExpand macro <$> (x:xs)))
macroExpand macro (Fix expr) = Fix (macroExpand macro <$> expr)

subst :: Macro a -> [InfiniteAST] -> InfiniteAST
subst macro args =
    let argMap = zip (params macro) args
    in  go argMap (result macro)
    where
        go argMap i@(Fix (ASTIdentifier ident)) =
            case lookup ident argMap of
                Just arg -> arg
                Nothing -> i
        go argMap (Fix expr) = Fix (go argMap <$> expr)