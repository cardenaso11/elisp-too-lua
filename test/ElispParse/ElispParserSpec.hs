{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

module ElispParse.ElispParserSpec
    ( spec
    )
where

import           Test.Hspec
import           Test.Hspec.Megaparsec
import           Text.Megaparsec               as M
import           Text.Megaparsec.Char
import qualified Data.Vector                   as V
import           Data.Void
import qualified Data.Text                     as T

import           ElispParse.Common
import           ElispParse.ElispParser

spec = do
    describe "parseProgram" $ do
        let runParseProgram = parseText parseProgram

        let oneTwoThree = (ASTInt <$> [1,2,3]) in do
            it "parses quoted expressions" $ do
                shouldParse (runParseProgram
                    "'(1 2 3)")
                    (ASTQuote oneTwoThree)
                shouldParse (runParseProgram
                    "'(a b c)")
                    (ASTQuote $ ASTIdentifier . Identifier <$> ["a","b","c"])
            it "parses nested quoted expressions" $ do
                shouldParse (runParseProgram
                    "'( '(1 2 3) '(1 2 3) '(1 2 3))")
                    (ASTQuote $ replicate 3 (ASTQuote oneTwoThree))
                shouldParse (runParseProgram
                    "'( (1 2 3) (1 2 3) (1 2 3))")
                    (ASTQuote $ replicate 3 (ASTList oneTwoThree))

        let fourFiveSix = ASTInt <$> [4,5,6] in do
            it "parses lists of expressions" $ do
                shouldParse (runParseProgram
                    "(4 5 6)")
                    (ASTList fourFiveSix)
            it "parses nested lists of expressions" $ do
                shouldParse (runParseProgram
                    "(   (4 5 6) (4 5 6) (4 5 6) )")
                    (ASTList $ replicate 3 (ASTList fourFiveSix))

        do
            it "parses backquoted expressions" $ do
                shouldParse (runParseProgram
                    "`(1 ,2 3)")
                    (ASTBackquote
                         [ Quoted (ASTInt 1)
                         , Unquoted (ASTInt 2)
                         , Quoted (ASTInt 3)
                         ])

            it "parses backquoted expressions containing identifiers" $ do
                shouldParse (runParseProgram
                    "`(1 ,2 abc)")
                    (ASTBackquote
                         [ Quoted (ASTInt 1)
                         , Unquoted (ASTInt 2)
                         , Quoted (ASTIdentifier (Identifier "abc"))
                         ])

        let emptyVector = ASTVector (HashableVector V.empty)
            zeroOneTwo = ASTVector (HashableVector (V.generate 3 ASTInt)) in do
            it "parses the empty vector" $ do
                shouldParse (runParseProgram
                    "[]")
                    emptyVector
            it "parses non-empty vectors" $ do
                shouldParse (runParseProgram
                    "[0 1 2]")
                    zeroOneTwo

        let hashConstructor = ASTIdentifier (Identifier "hash-table")
            table = ASTTable (hashConstructor : map ASTInt [1, 10, 2, 20]) in do
            it "parses tables" $ do
                shouldParse (runParseProgram
                    "#s(hash-table 1 10 2 20)")
                    table

        do
            it "parses identifiers" $ do
                shouldParse (runParseProgram
                    "garfield")
                    (ASTIdentifier (Identifier "garfield"))
