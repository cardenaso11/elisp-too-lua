{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module ElispParse.Common
    ( ASTVal (..)
    , HashableVector (..)
    , Identifier (..)
    , BackquotedAST (..)
    , Parser
    , RecursiveParser (..)
    , liftRP
    , runRP
    , mapAST
    , (|*>)
    , parseText
    , mfromMaybe
    , mfilter'
    , normalizeCase
    , normalizeCaseS
    , spaceConsumer
    , symbol
    , lexeme
    , parens
    , brackets ) where
import GHC.Generics
import qualified Data.Text as T
import Data.Hashable
import qualified Data.HashMap.Strict as HM
import Control.Exception
import Control.Applicative
import Control.Monad
import Control.Monad.Reader
import Data.Char
import Data.String
import Data.Monoid
import Data.Maybe
import qualified Data.Vector as V
import qualified Data.ByteString as BS
import qualified Data.Functor.Identity
import Data.Void
import Data.Proxy
import Text.Megaparsec as M
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L

--NOTE: not using classy mtl here on purpose: not worth effort
-- ^Parser is the type of a recursive parser
type Parser = Parsec Void T.Text
-- |RecursiveParser is the type of a recursive parser
newtype RecursiveParser a = RecursiveParser (ReaderT (Parser ASTVal) Parser a)
deriving newtype instance Functor RecursiveParser
deriving newtype instance Applicative RecursiveParser
deriving newtype instance Alternative RecursiveParser
deriving newtype instance Monad RecursiveParser
deriving newtype instance MonadPlus RecursiveParser
deriving newtype instance MonadReader (Parser ASTVal) RecursiveParser
deriving newtype instance MonadParsec Void T.Text RecursiveParser

-- ElVal only contans enough information to effeciently represent and
-- manipulate an elisp data structure. NOTE: this only represents an AST
-- therefore we dont put in any STRef s ElObjPtr stuff in here
data ASTVal = ASTList [ASTVal] -- TODO: add character tables
            | ASTQuote [ASTVal]
            | ASTBackquote BackquotedAST -- TODO: quasioquoting
            | ASTVector (HashableVector ASTVal)
            | ASTTable [ASTVal]
            | ASTCons [ASTVal] ASTVal
            | ASTIdentifier Identifier
            | ASTCharTable [ASTVal]
            | ASTCharSubTable [ASTVal]
            | ASTFloat Double -- praying emacs people didnt do anything weird
            | ASTInt Int
            | ASTChar Char
            | ASTString T.Text
            | ASTBoolVector Int T.Text
            | ASTByteCode [ASTVal] -- there really isnt much to do at parsing level
    deriving (Eq, Generic)

data BackquotedAST = Quoted ASTVal
                        | Unquoted ASTVal
                        | Spliced ASTVal
    deriving (Eq, Generic, Show, Hashable)

mapAST :: (ASTVal -> b) -> BackquotedAST -> b
mapAST f = \case
  Quoted a -> f a
  Unquoted a -> f a
  Spliced a -> f a

instance (IsString ASTVal) where
    fromString = ASTString . T.pack

-- TODO: existing bytecode is going to be hard. we can syntactically transpile
-- normal functions but any existing bytecode is going to be opaque.
-- we can either try to transpile it to lua as well,
-- or make our own jit for it with llvm-hs. making our own jit for it
-- seems superficially simple, but synchronizing state between them might
-- be difficult. on the other hand, transpiling to lua is going to probably
-- be very yucky and im not sure how fast it would be.
-- maybe try targetting luajit bytecode

deriving instance Show ASTVal
-- deriving instance Show ElObjPtr

-- -- make unique references
-- data ElObjPtr = ElObjPtr { pointerVal :: Int, dereference :: ElVal }
    -- deriving Generic

deriving instance Hashable ASTVal
-- deriving instance Hashable ElObjPtr

-- not sure if this is worth avoiding orphan instances
newtype HashableVector a = HashableVector (V.Vector a)
    deriving Eq

instance forall a. Show a => Show (HashableVector a)
    where
        show (HashableVector v) = show v
instance forall a. Hashable a => Hashable (HashableVector a)
    where
        hashWithSalt salt (HashableVector v) = hashWithSalt salt (V.toList v)

-- ellist = ElObjPtr . ElList

newtype Identifier = Identifier T.Text
    deriving (Eq, Show, Generic)

deriving anyclass instance Hashable Identifier


liftRP :: Parser a -> RecursiveParser a
liftRP = RecursiveParser . lift

runRP :: Parser ASTVal -> RecursiveParser a -> Parser a
runRP exp (RecursiveParser parser) = runReaderT parser exp

(|*>) :: forall m n . (Monad m, Monoid n) => m n -> m n -> m n
(|*>) = liftM2 (<>)

parseText :: forall a.
    Parser a
    -> T.Text
    -> Either (ParseError Char Void) a
parseText p = parse p ""

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

spaceConsumer :: (MonadParsec e s m, Token s ~ Char, Tokens s ~ T.Text) => m ()
spaceConsumer = L.space space1 lineComment blockComment
    where
    lineComment = L.skipLineComment (";" :: T.Text)
    blockComment = empty

lexeme :: MonadParsec e s m
  => Token s ~ Char
  => Tokens s ~ T.Text
  => m a -> m a
lexeme = L.lexeme spaceConsumer

symbol :: MonadParsec e s m
  => Token s ~ Char
  => Tokens s ~ T.Text
  => T.Text
  -> m T.Text
symbol = L.symbol spaceConsumer

parens :: MonadParsec e s m
  => Token s ~ Char
  => Tokens s ~ T.Text
  => m a -> m a
parens = between (symbol "(") (symbol ")")

brackets :: MonadParsec e s m
  => Token s ~ Char
  => Tokens s ~ T.Text
  => m a -> m a
brackets = between (symbol "[") (symbol "]")