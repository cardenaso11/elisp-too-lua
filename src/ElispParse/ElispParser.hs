{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE LambdaCase #-}

module ElispParse.ElispParser (parseProgram, exprFP) where

import GHC.Generics
import qualified Data.Text as T
import Data.Hashable
import qualified Data.HashMap.Strict as HM
import Control.Monad
import Control.Monad.Reader
import Control.Monad.ST.Strict
import Data.STRef.Strict
import Data.Monoid
import qualified Data.Vector as V
import qualified Data.ByteString as BS
import qualified Data.Functor.Identity
import Data.Void
import Data.Fixed
import Data.Proxy
import Text.Megaparsec as M
import Text.Megaparsec.Char
import Control.Lens
import qualified Text.Megaparsec.Char.Lexer as L

import ElispParse.Common
import ElispParse.NumberParser

import Debug.Trace

parseProgram :: Parser (AST InfiniteAST)
parseProgram = label "program" . between spaceConsumer eof . fmap ASTList $
  many exprFP

parseIdentifier :: Parser (AST a)
parseIdentifier = lexeme . label "identifier" $
  ASTIdentifier . Identifier <$> p
    where
    p = T.pack <$> some (choice [
        alphaNumChar,
        symbolChar,
        try $
          mfilter (not . flip elem
                   ['"'
                   , '\''
                   , ','
                   , '`'
                   , '('
                   , ')'
                   , '['
                   , ']']) punctuationChar])
-- what exactly is legal as an elisp identifier is ambigious
-- at best and unspecified at worst

parseList :: forall a. CompositeParser a
parseList recurse = lexeme . label "list" . fmap ASTList $
  parens (many recurse)

parseQuote :: forall a. CompositeParser a
parseQuote recurse = lexeme . label "quote" . fmap ASTQuote $
  char '\'' *> parens (many recurse)

parseBackquote :: forall a. CompositeParser a
parseBackquote bRecurse = lexeme . label "backquote" $
   ASTBackquote . Quoted <$>
   (char '`' *> parseList (parseBackquotedAST backquotedExprFP))
    where
        parseBackquotedAST recurse =  try (parseSpliced recurse)
                          <|> try (parseUnquoted recurse)
                          <|> try (parseQuoted recurse)
        parseSpliced :: Parser (AST (BackquotedAST a)) -> Parser (BackquotedAST a)
        parseSpliced recurse = fmap Spliced $ string (",@" :: T.Text) *> recurse

        parseUnquoted :: Parser (AST (BackquotedAST a)) -> ParsecT Void T.Text Identity (BackquotedAST a)
        parseUnquoted recurse = fmap Unquoted $ char ',' *> bRecurse

        parseQuoted :: Parser (AST (BackquotedAST a)) -> Parser (BackquotedAST a)
        parseQuoted recurse = Quoted <$> recurse

        backquotedExprFP :: Parser (AST (BackquotedAST a))
        backquotedExprFP = fix $ \e -> expr (parseBackquotedAST e)

parseVector :: forall a. CompositeParser a
parseVector recurse = lexeme . label "vector" $
  ASTVector . HashableVector . V.fromList <$> brackets (many recurse)

parseChar :: forall a. BaseParser a
parseChar = lexeme . label "character" . fmap ASTChar $
  char '?' *> L.charLiteral

parseString :: forall a. BaseParser a
parseString = lexeme . label "string" . fmap ASTString $
  char '"' *> (fmap T.pack . manyTill L.charLiteral $ char '"')

parseCons :: forall a. CompositeParser a
parseCons recurse = lexeme . label "cons" . parens $
    ASTCons <$> lexeme (someTill recurse (char '.')) <*> recurse

parseTable :: forall a. CompositeParser a
parseTable recurse = lexeme . label "table" . fmap ASTTable $
  string "#s" *> parens (some recurse)

parseCharTable :: forall a. CompositeParser a
parseCharTable recurse = lexeme . label "charTable" . fmap ASTCharTable $
  string "#^" *> brackets (many recurse)

parseCharSubTable :: forall a. CompositeParser a
parseCharSubTable recurse =
  lexeme . label "charSubTable" . fmap ASTCharSubTable $
  string "#^^" *> brackets (many recurse)

parseBoolVector :: forall a. BaseParser a
parseBoolVector = lexeme . label "boolVector" $
  string "#&"
  *> (ASTBoolVector
      <$> L.decimal
      <*> (parseString <&> \case (ASTString x) -> x))

parseByteCode :: forall a. CompositeParser a
parseByteCode recurse = lexeme . label "byteCode" $
    char '#' *> fmap ASTByteCode (brackets $ many recurse)

-- expr :: forall a. CompositeParser a
expr recurse =  let ar f = f recurse in
  choice $ try <$>
  [ ar parseCons
  , ar parseQuote
  , ar parseBackquote
  , ar parseList
  , ar parseVector
  , ar parseTable
  , ar parseCharTable
  , ar parseCharSubTable
  , ar parseByteCode
  , parseBoolVector
  , parseFloat
  , parseInt
  , parseChar
  , parseIdentifier
  , parseString
  ]
 -- we dont want to accidentally consume the integer part
 -- of a float as an integer or identifier, so parser order is important.
 -- alternative is to put notFollowedBy in parseInt
 -- and also identifier

exprFP :: Parser InfiniteAST
exprFP = fix $ \e -> Fix <$> expr e
