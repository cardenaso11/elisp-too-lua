{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE UndecidableInstances #-}

module ElispParse.ElispParser (parseProgram) where

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

--TODO: PLEASE refactor this

parseProgram :: Parser (ASTVal InfiniteAST)
parseProgram = label "program" . between spaceConsumer eof $ ASTList <$> many exprFP
    -- where
    --     f [x] = x
    --     f xs = ASTList xs

parseIdentifier :: Parser (ASTVal a)
parseIdentifier = lexeme . label "identifier" $ ASTIdentifier . Identifier <$> p
    where
    p = T.pack <$> some (choice [
        alphaNumChar,
        symbolChar,
        try $ mfilter (not . flip elem ['"', '\'', ',', '`', '(', ')', '[', ']']) punctuationChar])
        -- what exactly is legal as an elisp identifier is ambigious at best and unspecified at worst

parseList :: forall a. CompositeParser a
parseList recurse = lexeme . label "list" $ ASTList <$> parens (many recurse)

parseQuote :: forall a. CompositeParser a
parseQuote recurse = lexeme . label "quote" $ ASTQuote <$> (char '\'' *> parens (many recurse))

parseBackquote :: BaseParser a
parseBackquote = lexeme . label "backquote"  $
   --ASTQuote . fmap ASTBackquote
   ASTBackquote . Quoted <$> (char '`' *> parseList (parseBackquotedAST backquotedExprFP))
    where
        parseBackquotedAST recurse =  try (parseSpliced recurse)
                          <|> try (parseUnquoted recurse)
                          <|> try (parseQuoted recurse)
        parseSpliced recurse = fmap Spliced $ string (",@" :: T.Text) *> recurse
        parseUnquoted recurse = fmap (Unquoted . id) $ char ',' *> recurse
        parseQuoted recurse = Quoted <$> recurse
        backquotedExprFP = fix $ \e -> expr (parseBackquotedAST e)

parseVector :: CompositeParser a
parseVector recurse = lexeme . label "vector" $ ASTVector . HashableVector . V.fromList <$> brackets (many recurse)

parseChar :: BaseParser a
parseChar = lexeme . label "character" $ ASTChar <$> (char '?' *> L.charLiteral)

parseString :: BaseParser a
parseString = lexeme . label "string" $ ASTString <$> (char '"' *> (T.pack <$> manyTill L.charLiteral (char '"')))

parseCons :: CompositeParser a
parseCons recurse = lexeme . label "cons" . parens $
    ASTCons <$> lexeme (someTill recurse (char '.')) <*> recurse

parseTable :: CompositeParser a
parseTable recurse = lexeme . label "table" $ ASTTable <$> (string "#s" *> parens (some recurse))

parseCharTable :: CompositeParser a
parseCharTable recurse = lexeme . label "charTable" $ ASTCharTable <$> (string "#^" *> brackets (many recurse))

parseCharSubTable :: CompositeParser a
parseCharSubTable recurse = lexeme . label "charSubTable" $ ASTCharSubTable <$> (string "#^^" *> brackets (many recurse))

parseBoolVector :: BaseParser a
parseBoolVector = lexeme . label "boolVector" $ string "#&" *>
    (ASTBoolVector <$> L.decimal <*> (parseString <&> \case (ASTString x) -> x))

parseByteCode :: CompositeParser a
parseByteCode recurse = lexeme . label "byteCode" $
    char '#' *> (ASTByteCode <$> brackets (many recurse))

expr :: CompositeParser a
expr recurse =  let ar f = (f recurse) in
  choice $ try <$>
  [ ar parseCons
  , ar parseQuote
  , parseBackquote
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
  --     try $ applyRec parseCons
  -- <|> try (parseQuote recurse)
    -- <|> try (liftRP parseBackquote)
    -- <|> try parseList
    -- <|> try parseVector
    -- <|> try parseTable
    -- <|> try parseCharTable
    -- <|> try parseCharSubTable
    -- <|> try parseByteCode
    -- <|> try parseBoolVector
    -- <|> try (liftRP parseFloat)      -- we dont want to accidentally consume the integer part
    -- <|> try (liftRP parseInt)        -- of a float as an integer or identifier, so prioritize.
    -- <|> try parseChar                -- alternative is to put notFollowedBy in parseInt
    -- <|> try parseIdentifier          -- and also identifier
    -- <|> try parseString


exprFP :: Parser InfiniteAST
exprFP = fix $ \e -> Fix <$> expr e


newtype Fix a = Fix { unFix :: a (Fix a) }

instance (Show (a (Fix a))) => Show (Fix a) where
  show (Fix a) = show a

mapFix :: (a (Fix a) -> a1 (Fix a1)) -> Fix a -> Fix a1
mapFix f struct = Fix $ f (unFix struct)

--   show (Fix a) = show a
type InfiniteAST = Fix ASTVal
