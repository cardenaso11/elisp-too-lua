{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ScopedTypeVariables #-}

module ElispParse.NumberParserSpec (spec) where

import Test.Hspec
import Test.Hspec.Megaparsec
import Text.Megaparsec as M
import Text.Megaparsec.Char
import Data.Void
import qualified Data.Text as T

import ElispParse.Common
import ElispParse.NumberParser

parseText :: forall a.
    Parser a
    -> T.Text
    -> Either (ParseError Char Void) a
parseText p s = parse p "" s

spec = do
    describe "parseInt" $
        it "parses an arbitrary radix elisp int" $ do
            let runParseInt = parseText parseInt

            runParseInt "1" `shouldParse` (ASTInt 1)
            runParseInt "+1" `shouldParse` (ASTInt 1)
            runParseInt "-1" `shouldParse` (ASTInt (-1))
            runParseInt `shouldFailOn` "INF"

            let fourtyfour = ASTInt 44
            runParseInt "#b101100" `shouldParse` fourtyfour
            runParseInt "#o54" `shouldParse` fourtyfour
            runParseInt "#x2c" `shouldParse` fourtyfour
            runParseInt "#24r1k" `shouldParse` fourtyfour



    describe "parseFloat" $
        it "parses an elisp float" $ do
            let runParseFloat = parseText parseFloat

            runParseFloat "1.2" `shouldParse` ASTFloat 1.2

            let fifteenHundred = ASTFloat 1500.0
            runParseFloat "1500.0" `shouldParse` fifteenHundred
            runParseFloat "+15e2" `shouldParse` fifteenHundred
            runParseFloat "15.0e+2" `shouldParse` fifteenHundred
            runParseFloat "+1500000e-3" `shouldParse` fifteenHundred
            runParseFloat ".15e4" `shouldParse` fifteenHundred
            runParseFloat `shouldFailOn` "1500."
