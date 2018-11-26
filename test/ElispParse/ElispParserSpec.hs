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
        