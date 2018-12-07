{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE LambdaCase #-}

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

parseProgram :: Parser ASTVal
parseProgram = runRP exprFP . label "program" . between spaceConsumer eof $ f <$> many expr
    where
        f (x:[]) = x
        f xs = ASTList xs

-- parseIdentifier :: Parser 
-- parseIdentifier = ElIdentifier . Identifier <$> lexeme (p <?> "identifier")
--     where
--     p = T.pack <$>
--             (tokenToChunk (Proxy @String) <$> choice [letterChar, symbolChar]) -- apparently emacs lets you just start with numbers too
--             |*> many alphaNumChar
--     (|*>) = liftM2 (<>)

parseIdentifier :: RecursiveParser ASTVal
parseIdentifier = liftRP . lexeme . label "identifier" $ ASTIdentifier . Identifier <$> p
    where
    p = T.pack <$> some (choice [
        alphaNumChar,
        symbolChar,
        try $ mfilter (not . flip elem ['"', '\'', ',', '`', '(', ')', '[', ']']) punctuationChar])
        -- what exactly is legal as an elisp identifier is ambigious at best and unspecified at worst

parseList :: RecursiveParser ASTVal
parseList = ask >>= \recurse ->
  liftRP . lexeme . label "list" $ ASTList <$> parens (many recurse)

parseQuote :: RecursiveParser ASTVal
parseQuote = ask >>= \recurse ->
  liftRP . lexeme . label "quote" $ ASTQuote <$> (char '\'' *> parens (many recurse))

parseBackquote :: Parser ASTVal
parseBackquote = runRP backquotedExpr . lexeme . label "backquote"  $
   ASTBackquote <$> (char '`' *> parens (many parseBackquotedAST))
    where
        parseBackquotedAST =  try parseUnquoted
                          <|> try parseSpliced
                          <|> try parseQuoted
        parseUnquoted = do
          recurse <- ask
          fmap Unquoted $ char ',' *> liftRP recurse
        parseSpliced = do
          recurse <- ask
          fmap Spliced $ string ",@" *> liftRP recurse
        parseQuoted = do
          recurse <- ask
          Quoted <$> liftRP recurse
        backquotedExpr = fix $ \e -> ASTBackquote . pure <$> runRP e parseBackquotedAST

parseVector :: RecursiveParser ASTVal
parseVector = ask >>= \recurse ->
  liftRP . lexeme . label "vector" $ ASTVector . HashableVector . V.fromList <$> brackets (many recurse)

parseChar :: RecursiveParser ASTVal
parseChar = liftRP . lexeme . label "character" $ ASTChar <$> (char '?' *> L.charLiteral)

parseString :: RecursiveParser ASTVal
parseString = liftRP . lexeme . label "string" $ ASTString <$> (char '"' *> (T.pack <$> manyTill L.charLiteral (char '"')))

parseCons :: RecursiveParser ASTVal
parseCons = ask >>= \recurse ->
  liftRP . lexeme . label "cons" $
    parens (ASTCons <$> (lexeme $ someTill recurse (char '.')) <*> recurse)

parseTable :: RecursiveParser ASTVal
parseTable = ask >>= \recurse ->
  liftRP . lexeme . label "table" $ ASTTable <$> (string "#s" *> parens (some recurse))

parseCharTable :: RecursiveParser ASTVal
parseCharTable = ask >>= \recurse ->
  liftRP . lexeme . label "charTable" $ ASTCharTable <$> (string "#^" *> brackets (many recurse))

parseCharSubTable :: RecursiveParser ASTVal
parseCharSubTable = ask >>= \recurse ->
  liftRP . lexeme . label "charSubTable" $ ASTCharSubTable <$> (string "#^^" *> brackets (many recurse))

parseBoolVector :: RecursiveParser ASTVal
parseBoolVector = lexeme . label "boolVector" $ string "#&" *>
    (ASTBoolVector <$> L.decimal <*> (parseString <&> \case (ASTString x) -> x))

parseByteCode :: RecursiveParser ASTVal
parseByteCode = ask >>= \recurse ->
    liftRP . lexeme . label "byteCode" $
    char '#' *> (ASTByteCode <$> brackets (many recurse))

expr :: RecursiveParser ASTVal
expr =  try parseCons
    <|> try parseQuote
    <|> try (liftRP parseBackquote)
    <|> try parseList
    <|> try parseVector
    <|> try parseTable
    <|> try parseCharTable
    <|> try parseCharSubTable
    <|> try parseByteCode
    <|> try parseBoolVector
    <|> try (liftRP parseFloat)      -- we dont want to accidentally consume the integer part
    <|> try (liftRP parseInt)        -- of a float as an integer or identifier, so prioritize.
    <|> try parseChar                -- alternative is to put notFollowedBy in parseInt
    <|> try parseIdentifier          -- and also identifier
    <|> try parseString


exprFP :: Parser ASTVal
exprFP = fix $ \e -> runRP e expr
