{-# LANGUAGE OverloadedStrings #-}

module ElispParse.MacroExpansionSpec
    ( spec
    )
where

import Test.Hspec
import Text.RawString.QQ

import ElispParse.TestCommon
import ElispParse.Common
import ElispParse.MacroExpansion

spec = do
    let macroFoo = Macro (Identifier "foo") [Identifier "x", Identifier "y"] (fASTList [fASTIdentifier (Identifier "+"), fASTIdentifier (Identifier "y"), fASTIdentifier (Identifier "x")])
        macroBar = Macro (Identifier "bar") [] (fASTInt 0)
        macros = [macroFoo, macroBar]
    describe "macroExpand" $ do
        it "has no effect on expressions that don't contain a macro" $ do
            macroExpandWith macros (fASTInt 1)
                `shouldBe` (fASTInt 1)
        it "leaves unrelated identifiers alone" $ do
            macroExpandWith macros (fASTList [fASTIdentifier (Identifier "goo"), fASTList [fASTIdentifier (Identifier "bar")]])
                `shouldBe` (fASTList [fASTIdentifier (Identifier "goo"), fASTInt 0])
        it "expands macros" $ do
            macroExpandWith macros (fASTList [fASTIdentifier (Identifier "foo"), fASTInt 2, fASTIdentifier (Identifier "a")])
                `shouldBe` (fASTList [fASTIdentifier (Identifier "+"), fASTIdentifier (Identifier "a"), fASTInt 2])
    
    describe "autoMacroExpand" $ do
        let original = (fASTList [
                fASTList [
                    fASTIdentifier (Identifier "defmacro")
                  , fASTIdentifier (Identifier "foo")
                  , fASTList [fASTIdentifier (Identifier "x"), fASTIdentifier (Identifier "y")]
                  , fASTList [fASTIdentifier (Identifier "+"), fASTIdentifier (Identifier "y"), fASTIdentifier (Identifier "x")]
                  ]
              , fASTList [
                    fASTIdentifier (Identifier "foo")
                  , fASTList [fASTIdentifier (Identifier "a")]
                  , fASTList [fASTIdentifier (Identifier "b")]
                  ]
              ])
            expected = (fASTList [
                fASTList [
                    fASTIdentifier (Identifier "defmacro")
                  , fASTIdentifier (Identifier "foo")
                  , fASTList [fASTIdentifier (Identifier "x"), fASTIdentifier (Identifier "y")]
                  , fASTList [fASTIdentifier (Identifier "+"), fASTIdentifier (Identifier "y"), fASTIdentifier (Identifier "x")]
                  ]
              , fASTList [
                    fASTIdentifier (Identifier "+")
                  , fASTList [fASTIdentifier (Identifier "b")]
                  , fASTList [fASTIdentifier (Identifier "a")]
                  ]
              ])
        it "locates and expands macros" $ do
            macroExpand original `shouldBe` expected

        it "locates and expands nested macros" $ do
            macroExpand (fASTList [original]) `shouldBe` (fASTList [expected])