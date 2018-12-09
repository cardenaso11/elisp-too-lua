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
import qualified Data.Text as T

import ElispParse.Common
import ElispParse.NumberParser

shouldParse' :: Either (ParseError Char Void) InfiniteAST -> InfiniteAST -> Expectation
shouldParse' = shouldParse

fASTList :: [Fix ASTVal] -> Fix ASTVal
fASTList = Fix . ASTList

fASTQuote :: [Fix ASTVal] -> Fix ASTVal
fASTQuote = Fix . ASTQuote

fASTBackquote :: BackquotedAST -> Fix ASTVal
fASTBackquote = Fix . ASTBackquote

fASTVector :: HashableVector (Fix ASTVal) -> Fix ASTVal
fASTVector = Fix . ASTVector

fASTTable :: [Fix ASTVal] -> Fix ASTVal
fASTTable = Fix . ASTTable

fASTCons :: [Fix ASTVal] -> Fix ASTVal -> Fix ASTVal
fASTCons = (Fix .) . ASTCons

fASTIdentifier :: Identifier -> Fix ASTVal
fASTIdentifier = Fix . ASTIdentifier

fASTCharTable :: [Fix ASTVal] -> Fix ASTVal
fASTCharTable = Fix . ASTCharTable

fASTCharSubTable :: [Fix ASTVal] -> Fix ASTVal
fASTCharSubTable = Fix . ASTCharSubTable

fASTFloat :: Double -> Fix ASTVal
fASTFloat = Fix . ASTFloat

fASTInt :: Int -> Fix ASTVal
fASTInt = Fix . ASTInt

fASTChar :: Char -> Fix ASTVal
fASTChar = Fix . ASTChar

fASTString :: T.Text -> Fix ASTVal
fASTString = Fix . ASTString

fASTBoolVector :: Int -> T.Text -> Fix ASTVal
fASTBoolVector = (Fix .) . ASTBoolVector

fASTByteCode :: [Fix ASTVal] -> Fix ASTVal
fASTByteCode = Fix . ASTByteCode