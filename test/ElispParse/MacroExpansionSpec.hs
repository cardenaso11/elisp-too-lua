{-# LANGUAGE OverloadedStrings #-}

module ElispParse.MacroExpansionSpec
    ( spec
    )
where

import           Test.Hspec

import ElispParse.TestCommon
import ElispParse.Common
import ElispParse.MacroExpansion

spec = do
    let macroFoo = Macro (Identifier "foo") [Identifier "x", Identifier "y"] (fASTList [fASTIdentifier (Identifier "+"), fASTIdentifier (Identifier "y"), fASTIdentifier (Identifier "x")])
        macroBar = Macro (Identifier "bar") [] (fASTInt 0)
    describe "macroExpand" $ do
        it "has no effect on expressions that don't contain a macro" $ do
            macroExpand macroFoo (fASTInt 1)
                `shouldBe` (fASTInt 1)
        it "leaves unrelated identifiers alone" $ do
            macroExpand macroBar (fASTList [fASTIdentifier (Identifier "goo"), fASTList [fASTIdentifier (Identifier "bar")]])
                `shouldBe` (fASTList [fASTIdentifier (Identifier "goo"), fASTInt 0])
        it "expands macros" $ do
            macroExpand macroFoo (fASTList [fASTIdentifier (Identifier "foo"), fASTInt 2, fASTIdentifier (Identifier "a")])
                `shouldBe` (fASTList [fASTIdentifier (Identifier "+"), fASTIdentifier (Identifier "a"), fASTInt 2])