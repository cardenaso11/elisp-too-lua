{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE QuasiQuotes #-}

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
import qualified Data.Text.Lazy                as T
import Text.RawString.QQ

import ElispParse.TestCommon
import ElispParse.Common
import ElispParse.ElispParser

spec = do
    describe "parseProgram" $ do
        let runParseExprFP = parseText exprFP

        let oneTwoThree = (FASTInt <$> [1,2,3]) in do
            it "parses quoted expressions" $ do
                shouldParse' (runParseExprFP
                    "'(1 2 3)")
                    (FASTQuote . FASTList $ oneTwoThree)
                shouldParse' (runParseExprFP
                    "'(a b c)")
                    (FASTQuote . FASTList $ FASTIdentifier . Identifier <$> ["a","b","c"])
                shouldParse' (runParseExprFP
                     "'a")
                     (FASTQuote . FASTIdentifier . Identifier $ "a")
            it "parses nested quoted expressions" $ do
                shouldParse' (runParseExprFP
                    "'( '(1 2 3) '(1 2 3) '(1 2 3))")
                    (FASTQuote . FASTList $ replicate 3 (FASTQuote . FASTList $ oneTwoThree))
                shouldParse (runParseExprFP
                    "'( (1 2 3) (1 2 3) (1 2 3))")
                    (FASTQuote . FASTList $ replicate 3 (FASTList oneTwoThree))

        let fourFiveSix = FASTInt <$> [4,5,6] in do
            it "parses lists of expressions" $ do
                shouldParse' (runParseExprFP
                    "(4 5 6)")
                    (FASTList fourFiveSix)
            it "parses nested lists of expressions" $ do
                shouldParse' (runParseExprFP
                    "(   (4 5 6) (4 5 6) (4 5 6) )")
                    (FASTList $ replicate 3 (FASTList fourFiveSix))

        do
            it "parses backquoted expressions" $ do
                shouldParse' (runParseExprFP
                    "`(1 ,2 3)")
                    (FASTBackquote . Quoted . ASTList $
                         [ Quoted (ASTInt 1)
                         , Unquoted (FASTInt 2)
                         , Quoted (ASTInt 3)
                         ])

            it "parses backquoted expressions containing identifiers" $ do
                shouldParse' (runParseExprFP
                    "`(1 ,2 abc)")
                    (FASTBackquote . Quoted . ASTList $
                         [ Quoted (ASTInt 1)
                         , Unquoted (FASTInt 2)
                         , Quoted (ASTIdentifier (Identifier "abc"))
                         ])

            it "parses deeply nested backquoted expressions" $ do
                shouldParse' (runParseExprFP
                    "`(1 ,(+ 2 3) 4)")
                    (FASTBackquote . Quoted . ASTList $
                        [ Quoted (ASTInt 1)
                        , Unquoted (FASTList
                            [ FASTIdentifier (Identifier "+")
                            , FASTInt 2
                            , FASTInt 3
                            ])
                        , Quoted (ASTInt 4)
                        ])
            it "parses spliced expressions" $ do
                shouldParse' (runParseExprFP
                    "`(1 ,@(2 3 4) 5)")
                    (FASTBackquote . Quoted . ASTList $
                        [ Quoted (ASTInt 1)
                        , Spliced (FASTList
                            [ FASTInt 2
                            , FASTInt 3
                            , FASTInt 4
                            ])
                        , Quoted (ASTInt 5)
                        ])

        let emptyVector = FASTVector V.empty
            zeroOneTwo = FASTVector (V.generate 3 FASTInt) in do
            it "parses the empty vector" $ do
                shouldParse' (runParseExprFP
                    "[]")
                    emptyVector
            it "parses non-empty vectors" $ do
                shouldParse' (runParseExprFP
                    "[0 1 2]")
                    zeroOneTwo

        let hashConstructor = FASTIdentifier (Identifier "hash-table")
            table = FASTTable (hashConstructor : map FASTInt [1, 10, 2, 20]) in do
            it "parses tables" $ do
                shouldParse' (runParseExprFP
                    "#s(hash-table 1 10 2 20)")
                    table

        do
            it "parses identifiers" $ do
                shouldParse' (runParseExprFP
                    "garfield")
                    (FASTIdentifier (Identifier "garfield"))

        let a = FASTChar 'a'
            b = FASTChar 'b'
            c = FASTChar 'c'
            newlnC = FASTChar '\n'
            tabC = FASTChar '\t' in do
            it "parses character literals" $ do
                shouldParse' (runParseExprFP
                    "?a")
                    a
                shouldParse' (runParseExprFP
                    "?b")
                    b
                shouldParse' (runParseExprFP
                    "?c")
                    c

            it "parses escaped character literals" $ do
                shouldParse' (runParseExprFP
                    "?\\n")
                    newlnC
                shouldParse' (runParseExprFP
                    "?\\t")
                    tabC


        let owo = [r|
                so long suckers! i rev up my motorcylce and create a huge
                cloud of smoke. when the cloud dissipates im lying completely
                dead on the pavement|]
            string2 = [r|bc#5&\\2|]
            eee = [r|#&!\\\\><///|]
            escape = read . T.unpack
            quote t = T.cons '"' $ T.snoc t '"' in do

                it "parses string literals" $ do
                    shouldParse' (runParseExprFP $
                        quote owo)
                        (FASTString owo)
                    shouldParse' (runParseExprFP $
                        quote string2)
                        (FASTString . escape . quote $ string2)
                    shouldParse' (runParseExprFP $
                        quote eee)
                        (FASTString . escape . quote $ eee)

        it "parses escaped string literals" $ do
            shouldParse' (runParseExprFP
                [r|"\n"|])
                (FASTString "\n")
            shouldParse' (runParseExprFP
                [r|"\t"|])
                (FASTString "\t")


        let d = FASTIdentifier $ Identifier "d"
            one = FASTInt 1
            two = FASTInt 2 in do
            it "parses individual cons cells" $ do
                shouldParse' (runParseExprFP
                    "(1 . 2)")
                    (FASTCons [one] two)

            it "parses improper lists as cons cells" $ do
                shouldParse' (runParseExprFP
                    "(1 d . 2)")
                    (FASTCons [one, d] two)

            it "parses character tables" $ do
                shouldParse' (runParseExprFP
                    "#^[1 2 d]")
                    (FASTCharTable [one, two, d])

            it "parses character subtables" $ do
                shouldParse' (runParseExprFP
                    "#^^[d 2 1]")
                    (FASTCharSubTable [d, two, one])

        it "parses boolvector" $ do
            shouldParse' (runParseExprFP
                [r|#&4"hewwo"|])
                (FASTBoolVector 4 "hewwo")

        it "parses bytecode" $ do
            shouldParse' (runParseExprFP
                "#[1 2 3 4]")
                (FASTByteCode $ FASTInt <$> [1, 2, 3, 4])
