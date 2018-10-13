{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE LambdaCase #-}

module ElispParse.Common
    ( ElVal (..)
    , Parser
    , Identifier (..)
    , (|*>)
    , mfromMaybe
    , mfilter'
    , normalizeCase
    , normalizeCaseS
    , spaceConsumer
    , lexeme
    , parens ) where
import GHC.Generics
import qualified Data.Text as T
import Data.Hashable
import qualified Data.HashMap.Strict as HM
import Control.Exception
import Control.Monad
import Data.Char
import Data.Monoid
import Data.Maybe
import qualified Data.Functor.Identity
import Data.Void
import Data.Proxy
import Text.Megaparsec as M
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L

type Parser = Parsec Void T.Text

data ElVal = ElList [ElVal] -- TODO: add character tables
            | ElBackquote [ElVal]    
            | ElVector [ElVal]
            | ElTable (HM.HashMap ElVal ElVal)
            | ElImproperList [ElVal] ElVal
            | ElIdentifier Identifier
            | ElFloat Double -- praying emacs people didnt do anything weird
            | ElInt Int
            | ElString T.Text
    deriving (Show, Generic)

-- make unique references
data ElObjPtr = ElObjPtr { pointerVal :: Int, dereference :: ElVal }

deriving instance Hashable ElVal

newtype Identifier = Identifier T.Text
    deriving (Show, Generic)

deriving instance Hashable Identifier

(|*>) :: forall m n . (Monad m, Monoid n) => m n -> m n -> m n
(|*>) = liftM2 (<>)


-- probably want to migrate away from stack eventually so i can just pull in monadplus
mfromMaybe :: MonadPlus m => Maybe a -> m a
mfromMaybe = maybe mzero pure

mfilter' :: forall a m. MonadPlus m => Bool -> a -> m a
mfilter' p x = if p then pure x else mzero

normalizeCase :: Char -> Char
normalizeCase = toLower

normalizeCaseS :: String -> String -- convention here will be to lowercase
normalizeCaseS = fmap normalizeCase

specialForms :: [T.Text] -- i doubt the parser is gonna use these but im not compiling this list again lol
specialForms = ["and", "catch", "cond", "condition-case", "defconst",
    "defvar", "function", "if", "interactive", "lambda", "let", "let*",
    "or", "prog1", "prog2", "progn", "quote", "save-current-buffer",
    "save-excursion", "save-restriction", "setq", "setq-default",
    "track-mouse", "unwind-protect", "while"]

spaceConsumer :: Parser ()
spaceConsumer = L.space space1 lineComment blockComment
    where
    lineComment = L.skipLineComment ";"
    blockComment = empty

lexeme :: forall a. Parser a -> Parser a
lexeme = L.lexeme spaceConsumer

symbol :: T.Text -> Parser T.Text
symbol = L.symbol spaceConsumer

parens :: forall a. Parser a -> Parser a
parens = between (symbol "(") (symbol ")")