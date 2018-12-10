{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances #-}

module ElispParse.Common
    ( AST (..)
    , HashableVector (..)
    , Identifier (..)
    , BackquotedAST (..)
    , Parser
    , BaseParser
    , CompositeParser
    , Fix (..)
    , InfiniteAST
    , InfiniteBackquotedAST
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

-- | The type of our parser.
--   Note that there are no error messages for now, hence, Void for errors
type Parser = Parsec Void T.Text

-- | A terminal parser, that doesn't recurse
type BaseParser a = Parser (AST a)
-- | A parser that recurses
type CompositeParser a = Parser a -> Parser (AST a)

-- | An elisp AST node.
--   Note the polymorphism in its coinduction:
--   this is primarily so we can guarantee in the types backquoted elements
--   can only appear inside BackquotedAST
data AST a = ASTList [a]
              | ASTQuote [a]
              | ASTBackquote (BackquotedAST a)
              | ASTVector (HashableVector a)
              | ASTTable [a]
              | ASTCons [a] a
              | ASTIdentifier Identifier
              | ASTCharTable [a]
              | ASTCharSubTable [a]
              | ASTFloat Double -- praying emacs people didnt do anything weird
              | ASTInt Int
              | ASTChar Char
              | ASTString T.Text
              | ASTBoolVector Int T.Text
              | ASTByteCode [a] -- there really isnt much to do at parsing level
    deriving (Eq, Generic)

-- | A backquoted AST node. Note its polymorphism in its coinduction:
--   this allows us to guarantee in the types that once we leave the backquote
--   with Unquoted we cannot re-quote or splice
data BackquotedAST a = Quoted (AST (BackquotedAST a))
                     | Unquoted a
                     | Spliced (AST (BackquotedAST a))
    deriving (Eq, Generic, Show, Hashable)

-- | Type level fixed-point combinator
newtype Fix a = Fix { unFix :: a (Fix a) }

instance (IsString (AST a)) where
    fromString = ASTString . T.pack
instance (Show (a (Fix a))) => Show (Fix a) where
  show (Fix a) = show a
deriving newtype instance (Eq (a (Fix a))) => Eq (Fix a)
deriving newtype instance (Hashable (a (Fix a))) => Hashable (Fix a)

-- | An AST node that is guaranteed to only contain other AST nodes, ending
--   in one of the AST terminals (literals)
type InfiniteAST = Fix AST
type InfiniteBackquotedAST = Fix BackquotedAST

-- TODO: existing bytecode is going to be hard. we can syntactically transpile
-- normal functions but any existing bytecode is going to be opaque.
-- we can either try to transpile it to lua as well,
-- or make our own jit for it with llvm-hs. making our own jit for it
-- seems superficially simple, but synchronizing state between them might
-- be difficult. on the other hand, transpiling to lua is going to probably
-- be very yucky and im not sure how fast it would be.
-- maybe try targetting luajit bytecode

deriving instance Show a => Show (AST a)

deriving instance Hashable a => Hashable (AST a)

-- not sure if this is worth avoiding orphan instances
-- | A newtype for a Hashable Vector of Hashable elements.
--   This exists to not have an orphaned instance.
newtype HashableVector a = HashableVector (V.Vector a)
  deriving Eq
instance forall a. Show a => Show (HashableVector a) where
  show (HashableVector v) = show v
instance forall a. Hashable a => Hashable (HashableVector a) where
  hashWithSalt salt (HashableVector v) = hashWithSalt salt (V.toList v)

-- | An elisp identifier
newtype Identifier = Identifier T.Text
    deriving (Eq, Show, Generic)

deriving anyclass instance Hashable Identifier

-- | Parse some text into an AST value or an error
parseText :: forall a.
    Parser a
    -> T.Text
    -> Either (ParseError Char Void) a
parseText p = parse p ""

-- probably want to migrate away from stack eventually so
-- we can just pull in monadplus
mfromMaybe :: MonadPlus m => Maybe a -> m a
mfromMaybe = maybe mzero pure

mfilter' :: forall a m. MonadPlus m => Bool -> a -> m a
mfilter' p x = if p then pure x else mzero

-- used to normalize case inside our parser
normalizeCase :: Char -> Char
normalizeCase = toLower

-- normalize case of a whole string
normalizeCaseS :: String -> String -- convention here will be to lowercase
normalizeCaseS = fmap normalizeCase

-- i doubt the parser is gonna use these but im not compiling this list again lol
specialForms :: [T.Text]
specialForms = ["and", "catch", "cond", "condition-case", "defconst",
    "defvar", "function", "if", "interactive", "lambda", "let", "let*",
    "or", "prog1", "prog2", "progn", "quote", "save-current-buffer",
    "save-excursion", "save-restriction", "setq", "setq-default",
    "track-mouse", "unwind-protect", "while"]

-- consume elisp spaces and comments
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

-- | Elisp tokenizer
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
