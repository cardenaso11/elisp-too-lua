{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}

module ElispParse.ElispParser (parseProgram) where

import GHC.Generics
import qualified Data.Text as T
import Data.Hashable
import qualified Data.HashMap.Strict as HM
import Control.Monad
import Control.Monad.ST.Strict
import Data.STRef.Strict
import Data.Monoid
import qualified Data.Vector as V
import qualified Data.ByteString as BS
import qualified Data.Functor.Identity
import Data.Void
import Data.Proxy
import Text.Megaparsec as M
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L

import ElispParse.Common
import ElispParse.NumberParser

import Debug.Trace

parseProgram :: Parser ASTVal
parseProgram = label "program" . between spaceConsumer eof $ f <$> many expr
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

parseIdentifier :: Parser ASTVal
parseIdentifier = lexeme . label "identifier" $ ASTIdentifier . Identifier <$> p
    where
    p = T.pack <$> some (choice [
        alphaNumChar,
        symbolChar,
        mfilter (not . flip elem ['"', '\'', ',', '`', '(', ')', '[', ']']) punctuationChar])
        -- what exactly is legal as an elisp identifier is ambigious at best and unspecified at worst

parseList :: Parser ASTVal
parseList = lexeme . label "list" $ ASTList <$> parens (many expr)

parseQuote :: Parser ASTVal
parseQuote = lexeme . label "quote" $ ASTQuote <$> (char '\'' *> parens (many expr))

parseBackquote :: Parser ASTVal
parseBackquote = lexeme . label "backquote" $ ASTBackquote <$> (char '`' *> parens (many parseBackquoteElement))
    where
        parseBackquoteElement =  try parseUnquoted
                            <|> try parseSpliced
                            <|> try parseQuoted
        parseUnquoted = Unquoted <$> (char ',' *> expr)
        parseSpliced = Spliced <$> (string ",@" *> expr)
        parseQuoted = Quoted <$> expr 

parseVector :: Parser ASTVal
parseVector = lexeme . label "vector" $ ASTVector . HashableVector . V.fromList <$> between (symbol "[") (symbol "]") (many expr)

parseString :: Parser ASTVal
parseString = lexeme . label "string" $ ASTString <$> (char '"' *> (T.pack <$> many (noneOf ['"'])) <* char '"')

parseCons :: Parser ASTVal
parseCons = lexeme . label "cons" $
    parens $ ASTCons <$> some expr <* (lexeme $ char '.') <*> expr

parseTable :: Parser ASTVal
parseTable = lexeme . label "table" $ ASTTable <$> (string "#s" *> parens (some expr))


expr :: Parser ASTVal
expr =  try parseQuote
    <|> try parseBackquote
    <|> try parseList
    <|> try parseCons
    <|> try parseVector
    <|> try parseTable
    <|> try parseFloat      -- we dont want to accidentally consume the integer part
    <|> try parseInt        -- of a float as an integer or identifier, so prioritize.
    <|> try parseIdentifier -- alternative is to put notFollowedBy in parseInt
    <|> try parseString     -- and also identifier