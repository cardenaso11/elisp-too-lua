{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE UndecidableInstances #-}

module ElispParse.PrettyPrint where

import ElispParse.Common
import Data.Foldable
import Data.Text.Prettyprint.Doc

instance Pretty (f (Fix f)) => Pretty (Fix f) where
    pretty = pretty . unFix

instance Pretty a => Pretty (AST a) where
    pretty = \case
        ASTList xs     -> "(" <> hsep (pretty <$> xs) <> ")"
        ASTQuote x     -> "'" <> pretty x
        ASTBackquote x -> "`" <> pretty x
        ASTInt n       -> pretty n
        ASTChar c      -> "'" <> pretty c <> "'"
        ASTString s    -> "\"" <> pretty s <> "\""
        ASTIdentifier (Identifier i) -> pretty i

instance Pretty a => Pretty (BackquotedAST a) where
    pretty = \case
        Quoted   ast -> pretty ast
        Unquoted ast -> "," <> pretty ast
        Spliced  ast -> ",@" <> pretty ast
