{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

module ElispParse.TestCommon
  ( shouldParse'
  , fASTList
  , fASTQuote
  , fASTBackquote
  , fASTVector
  , fASTTable
  , fASTCons
  , fASTIdentifier
  , fASTCharTable
  , fASTCharSubTable
  , fASTFloat
  , fASTInt
  , fASTChar
  , fASTString
  , fASTBoolVector
  , fASTByteCode
  ) where

import Test.Hspec
import Test.Hspec.Megaparsec
import Text.Megaparsec as M
import Text.Megaparsec.Error
import Text.Megaparsec.Char
import Data.Void
import qualified Data.Text.Lazy as T

import ElispParse.Common
import ElispParse.NumberParser

shouldParse' :: Either (ParseError Char Void) InfiniteAST -> InfiniteAST -> Expectation
shouldParse' = shouldParse

fASTList :: [Fix AST] -> Fix AST
fASTList = Fix . ASTList

fASTQuote :: Fix AST -> Fix AST
fASTQuote = Fix . ASTQuote

fASTBackquote :: BackquotedAST (Fix AST) -> Fix AST
fASTBackquote = Fix . ASTBackquote

fASTVector :: HashableVector (Fix AST) -> Fix AST
fASTVector = Fix . ASTVector

fASTTable :: [Fix AST] -> Fix AST
fASTTable = Fix . ASTTable

fASTCons :: [Fix AST] -> Fix AST -> Fix AST
fASTCons = (Fix .) . ASTCons

fASTIdentifier :: Identifier -> Fix AST
fASTIdentifier = Fix . ASTIdentifier

fASTCharTable :: [Fix AST] -> Fix AST
fASTCharTable = Fix . ASTCharTable

fASTCharSubTable :: [Fix AST] -> Fix AST
fASTCharSubTable = Fix . ASTCharSubTable

fASTFloat :: Double -> Fix AST
fASTFloat = Fix . ASTFloat

fASTInt :: Int -> Fix AST
fASTInt = Fix . ASTInt

fASTChar :: Char -> Fix AST
fASTChar = Fix . ASTChar

fASTString :: T.Text -> Fix AST
fASTString = Fix . ASTString

fASTBoolVector :: Int -> T.Text -> Fix AST
fASTBoolVector = (Fix .) . ASTBoolVector

fASTByteCode :: [Fix AST] -> Fix AST
fASTByteCode = Fix . ASTByteCode