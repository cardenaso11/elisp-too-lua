{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
-- {-# LANGUAGE GeneralizedNewtypeDeriving #-}

module ElispParse.ElispParser () where

import GHC.Generics
import qualified Data.Text as T
import Data.Hashable
import qualified Data.HashMap.Strict as HM
import Control.Monad
import Control.Monad.ST.Strict
import Data.STRef.Strict
import Data.Monoid
import qualified Data.Functor.Identity
import Data.Void
import Data.Proxy
import Text.Megaparsec as M
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L

import ElispParse.Common
import ElispParse.NumberParser

parseProgram :: Parser ElVal
parseProgram = label "program" . between spaceConsumer eof $ f <$> many expr
    where
        f (x:[]) = x
        f xs = ElList xs

-- parseIdentifier :: Parser ElVal
-- parseIdentifier = ElIdentifier . Identifier <$> lexeme (p <?> "identifier")
--     where
--     p = T.pack <$>
--             (tokenToChunk (Proxy @String) <$> choice [letterChar, symbolChar]) -- apparently emacs lets you just start with numbers too
--             |*> many alphaNumChar
--     (|*>) = liftM2 (<>)

parseIdentifier :: Parser ElVal
parseIdentifier = ElIdentifier . Identifier <$> lexeme (p <?> "identifier")
    where
    p = T.pack <$> many (choice [alphaNumChar, symbolChar]) -- might regret this if symbolchar conflicts with # reader syntax

parseList :: Parser ElVal
parseList = ElList <$> lexeme (parens (many expr) <?> "list")

parseQuote :: Parser ElVal
parseQuote = ElList <$> lexeme ((char '\'' *> parens (many expr)) <?> "quote")


-- parseFloat :: Parser ElVal
-- parseFloat = ElFloat <$> L.float
-- parseFloat = ElFloat . read <$> lexeme getFloatString <?> "float"
--     where
--         getFloatString = some digitChar *> 

parseString :: Parser ElVal
parseString = ElString <$> lexeme (char '"' *> (T.pack <$> many (noneOf ['"'])) <* char '"' <?> "string")

expr :: Parser ElVal
expr = try parseQuote
    <|> try parseList
    <|> try parseIdentifier
    -- <|> try parseInt
    -- <|> try parseFloat
    <|> try parseString