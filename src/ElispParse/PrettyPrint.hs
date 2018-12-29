{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module ElispParse.PrettyPrint where

import ElispParse.Common
import Data.Foldable
import qualified Data.Text.Lazy as T
import Data.Text.Prettyprint.Doc

instance Pretty InfiniteAST where
    pretty = \case
        FASTList xs
            | any isList xs -> hang 2 $ "(" <> vsep (pretty <$> xs) <> ")"
            where
                isList (FASTList _) = True
                isList _            = False
        Fix x -> pretty x

instance Pretty a => Pretty (AST a) where
    pretty = \case
        ASTList xs                   ->          parens xs
        ASTQuote x                   -> "'"   <> pretty x
        ASTBackquote x               -> "`"   <> pretty x
        ASTVector (HashableVector v) ->          brackets (toList v)
        ASTTable xs                  -> "#s"  <> parens xs
        ASTCons xs x                 ->          cons xs x
        ASTCharTable xs              -> "#^"  <> brackets xs
        ASTCharSubTable xs           -> "#^^" <> brackets xs
        ASTByteCode xs               -> "#"   <> brackets xs
        ASTIdentifier (Identifier i) -> pretty i
        ASTFloat f                   -> pretty f
        ASTInt n                     -> pretty n
        ASTChar c                    -> char c
        ASTString s                  -> escapedText s
        ASTBoolVector n t            -> "#&" <> pretty n <> escapedText t
        where
            parens xs   = "(" <> hsep (pretty <$> xs) <> ")"
            brackets xs = "[" <> hsep (pretty <$> xs) <> "]"
            escapedText s = "\"" <> pretty (escapeText s) <> "\""
            char c = "?" <> pretty c
            cons xs x = "(" <> hsep (fmap pretty xs ++ [".", pretty x]) <> ")"

instance Pretty a => Pretty (BackquotedAST a) where
    pretty = \case
        Quoted   ast -> pretty ast
        Unquoted ast -> "," <> pretty ast
        Spliced  ast -> ",@" <> pretty ast

-- | Produce a Doc from a complete elisp program by printing each top-level
-- declaration on a new line.
program :: InfiniteAST -> Maybe (Doc a)
program (FASTList xs) = Just (vsep (pretty <$> xs))
program _            = Nothing

escapeText :: T.Text -> T.Text
escapeText = T.concatMap \case
    '"' -> "\\\""
    c   -> T.singleton c
