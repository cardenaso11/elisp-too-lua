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
    let macroFoo = Macro (Identifier "foo") [Identifier "x", Identifier "y"] (FASTList [FASTIdentifier (Identifier "+"), FASTIdentifier (Identifier "y"), FASTIdentifier (Identifier "x")])
        macroBar = Macro (Identifier "bar") [] (FASTInt 0)
        macros = [macroFoo, macroBar]
    describe "macroExpandWith" $ do
        it "has no effect on expressions that don't contain a macro" $ do
            macroExpandWith macros (FASTInt 1)
                `shouldBe` (FASTInt 1)
        it "leaves unrelated identifiers alone" $ do
            macroExpandWith macros (FASTList [FASTIdentifier (Identifier "goo"), FASTList [FASTIdentifier (Identifier "bar")]])
                `shouldBe` (FASTList [FASTIdentifier (Identifier "goo"), FASTInt 0])
        it "expands macros" $ do
            macroExpandWith macros (FASTList [FASTIdentifier (Identifier "foo"), FASTInt 2, FASTIdentifier (Identifier "a")])
                `shouldBe` (FASTList [FASTIdentifier (Identifier "+"), FASTIdentifier (Identifier "a"), FASTInt 2])
    
    describe "macroExpand" $ do
        let original = (FASTList [
                FASTList [
                    FASTIdentifier (Identifier "defmacro")
                  , FASTIdentifier (Identifier "foo")
                  , FASTList [FASTIdentifier (Identifier "x"), FASTIdentifier (Identifier "y")]
                  , FASTList [FASTIdentifier (Identifier "+"), FASTIdentifier (Identifier "y"), FASTIdentifier (Identifier "x")]
                  ]
              , FASTList [
                    FASTIdentifier (Identifier "foo")
                  , FASTList [FASTIdentifier (Identifier "a")]
                  , FASTList [FASTIdentifier (Identifier "b")]
                  ]
              ])
            expected = (FASTList [
                FASTList [
                    FASTIdentifier (Identifier "defmacro")
                  , FASTIdentifier (Identifier "foo")
                  , FASTList [FASTIdentifier (Identifier "x"), FASTIdentifier (Identifier "y")]
                  , FASTList [FASTIdentifier (Identifier "+"), FASTIdentifier (Identifier "y"), FASTIdentifier (Identifier "x")]
                  ]
              , FASTList [
                    FASTIdentifier (Identifier "+")
                  , FASTList [FASTIdentifier (Identifier "b")]
                  , FASTList [FASTIdentifier (Identifier "a")]
                  ]
              ])
        it "locates and expands macros" $ do
            macroExpand original `shouldBe` expected

        it "locates and expands nested macros" $ do
            macroExpand (FASTList [original]) `shouldBe` (FASTList [expected])

        it "fully expands expressions requiring repeated expansion" $ do
            let original = FASTList [
                    FASTList [
                        FASTIdentifier (Identifier "defmacro")
                      , FASTIdentifier (Identifier "f")
                      , FASTList [FASTIdentifier (Identifier "n")]
                      , FASTList [
                            FASTIdentifier (Identifier "*")
                          , FASTIdentifier (Identifier "n")
                          , FASTList [
                                FASTIdentifier (Identifier "g")
                              , FASTIdentifier (Identifier "n")
                              ]
                          ]
                      ]
                  , FASTList [
                        FASTIdentifier (Identifier "defmacro")
                      , FASTIdentifier (Identifier "g")
                      , FASTList [FASTIdentifier (Identifier "n")]
                      , FASTList [
                            FASTIdentifier (Identifier "+")
                          , FASTIdentifier (Identifier "n")
                          , FASTIdentifier (Identifier "1")
                          ]
                      ]
                  , FASTList [
                        FASTIdentifier (Identifier "f")
                      , FASTIdentifier (Identifier "1")
                      ]
                  ]
            let expected = FASTList [
                    FASTList [
                        FASTIdentifier (Identifier "defmacro")
                      , FASTIdentifier (Identifier "f")
                      , FASTList [FASTIdentifier (Identifier "n")]
                      , FASTList [
                            FASTIdentifier (Identifier "*")
                          , FASTIdentifier (Identifier "n")
                          , FASTList [
                                FASTIdentifier (Identifier "g")
                              , FASTIdentifier (Identifier "n")
                              ]
                          ]
                      ]
                  , FASTList [
                        FASTIdentifier (Identifier "defmacro")
                      , FASTIdentifier (Identifier "g")
                      , FASTList [FASTIdentifier (Identifier "n")]
                      , FASTList [
                            FASTIdentifier (Identifier "+")
                          , FASTIdentifier (Identifier "n")
                          , FASTIdentifier (Identifier "1")
                          ]
                      ]
                  , FASTList [
                        FASTIdentifier (Identifier "*")
                      , FASTIdentifier (Identifier "1")
                      , FASTList [
                            FASTIdentifier (Identifier "+")
                          , FASTIdentifier (Identifier "1")
                          , FASTIdentifier (Identifier "1")
                          ]
                      ]
                  ]
            macroExpand original `shouldBe` expected