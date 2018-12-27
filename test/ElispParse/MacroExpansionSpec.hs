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
    let macroFoo =
          let mName = Identifier "foo"
              mRegularParams = [Identifier "x", Identifier "y"]
              mOptionalParams = []
              mRestParam = Nothing
              mResult = FASTList
                [
                  FASTIdentifier_ "+"
                , FASTIdentifier_ "y"
                , FASTIdentifier_ "x"
                ]
          in Macro { name = mName
                   , regularParams = mRegularParams
                   , optionalParams = mOptionalParams
                   , restParam = mRestParam
                   , result = mResult
                   }
        macroBar = Macro (Identifier "bar") [] [] Nothing (FASTInt 0)
        macros = [macroFoo, macroBar]
    describe "macroExpandWith" $ do
        it "has no effect on expressions that don't contain a macro" $ do
            macroExpandWith macros (FASTInt 1)
                `shouldBe` Just (FASTInt 1)
        it "leaves unrelated identifiers alone" $ do
            macroExpandWith macros (FASTList [FASTIdentifier_ "goo", FASTList [FASTIdentifier (Identifier "bar")]])
                `shouldBe` Just (FASTList [FASTIdentifier_ "goo", FASTInt 0])
        it "expands macros" $ do
            macroExpandWith macros (FASTList [FASTList [FASTIdentifier_ "foo", FASTInt 2, FASTIdentifier (Identifier "a")]])
                `shouldBe` Just (FASTList [FASTList [FASTIdentifier_ "+", FASTIdentifier (Identifier "a"), FASTInt 2]])

    describe "macroExpand" $ do
        let original = (FASTList [
                FASTList [
                    FASTIdentifier_ "defmacro"
                  , FASTIdentifier_ "foo"
                  , FASTList [FASTIdentifier_ "x", FASTIdentifier_ "y"]
                  , FASTList [FASTIdentifier_ "+", FASTIdentifier_ "y", FASTIdentifier_ "x"]
                  ]
              , FASTList [
                    FASTIdentifier_ "foo"
                  , FASTList [FASTIdentifier_ "a"]
                  , FASTList [FASTIdentifier_ "b"]
                  ]
              ])
            expected = (FASTList [
                FASTList [
                    FASTIdentifier_ "defmacro"
                  , FASTIdentifier_ "foo"
                  , FASTList [FASTIdentifier_ "x", FASTIdentifier_ "y"]
                  , FASTList [FASTIdentifier_ "+", FASTIdentifier_ "y", FASTIdentifier_ "x"]
                  ]
              , FASTList [
                    FASTIdentifier_ "+"
                  , FASTList [FASTIdentifier_ "b"]
                  , FASTList [FASTIdentifier_ "a"]
                  ]
              ])
        it "locates and expands macros" $ do
            macroExpand original `shouldBe` Just expected

        it "shouldn't find nested macro definitions" $ do
            macroExpand (FASTList [original]) `shouldBe` Just (FASTList [original])

        it "fully expands expressions requiring repeated expansion" $ do
            let original = FASTList [
                    FASTList [
                        FASTIdentifier_ "defmacro"
                      , FASTIdentifier_ "f"
                      , FASTList [FASTIdentifier_ "n"]
                      , FASTList [
                            FASTIdentifier_ "*"
                          , FASTIdentifier_ "n"
                          , FASTList [
                                FASTIdentifier_ "g"
                              , FASTIdentifier_ "n"
                              ]
                          ]
                      ]
                  , FASTList [
                        FASTIdentifier_ "defmacro"
                      , FASTIdentifier_ "g"
                      , FASTList [FASTIdentifier_ "n"]
                      , FASTList [
                            FASTIdentifier_ "+"
                          , FASTIdentifier_ "n"
                          , FASTIdentifier_ "1"
                          ]
                      ]
                  , FASTList [
                        FASTIdentifier_ "f"
                      , FASTIdentifier_ "1"
                      ]
                  ]
            let expected = FASTList [
                    FASTList [
                        FASTIdentifier_ "defmacro"
                      , FASTIdentifier_ "f"
                      , FASTList [FASTIdentifier_ "n"]
                      , FASTList [
                            FASTIdentifier_ "*"
                          , FASTIdentifier_ "n"
                          , FASTList [
                                FASTIdentifier_ "g"
                              , FASTIdentifier_ "n"
                              ]
                          ]
                      ]
                  , FASTList [
                        FASTIdentifier_ "defmacro"
                      , FASTIdentifier_ "g"
                      , FASTList [FASTIdentifier_ "n"]
                      , FASTList [
                            FASTIdentifier_ "+"
                          , FASTIdentifier_ "n"
                          , FASTIdentifier_ "1"
                          ]
                      ]
                  , FASTList [
                        FASTIdentifier_ "*"
                      , FASTIdentifier_ "1"
                      , FASTList [
                            FASTIdentifier_ "+"
                          , FASTIdentifier_ "1"
                          , FASTIdentifier_ "1"
                          ]
                      ]
                  ]
            macroExpand original `shouldBe` Just expected
