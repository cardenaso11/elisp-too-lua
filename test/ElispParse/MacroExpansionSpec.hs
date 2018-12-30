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
            mResult = FAL
              [
                FAId_ "+"
              , FAId_ "y"
              , FAId_ "x"
              ]
        in Macro { name = mName
                  , regularParams = mRegularParams
                  , optionalParams = mOptionalParams
                  , restParam = mRestParam
                  , result = mResult
                  }
      macroBar = Macro (Identifier "bar") [] [] Nothing (FAIn 0)
      macros = [macroFoo, macroBar]


  describe "toMacro" $ do

    it "parses a defmacro macro declaration from an InfiniteAST" $ do
      let simpleMacro =
            let mName = Identifier "simple-add-three"
                mRegularParams = Identifier <$> ["a", "b", "c"]
                mOptionalParams = []
                mRestParam = Nothing
                mResult = FAL
                  [ FAId_ "+"
                  , FAId_ "a"
                  , FAId_ "b"
                  , FAId_ "c"
                  ]
            in  Macro { name = mName
                      , regularParams = mRegularParams
                      , optionalParams = mOptionalParams
                      , restParam = mRestParam
                      , result = mResult
                      }
      toMacro
        (FAL
          [ FAId_ "defmacro"
          , FAId_ "simple-add-three"
          , FAL $ FAId_ <$> ["a", "b", "c"]
          , FAL
            [ FAId_ "+"
            , FAId_ "a"
            , FAId_ "b"
            , FAId_ "c"
            ]
          ]) `shouldBe` Just simpleMacro

    it "parses a defmacro macro declaration with optional arguments" $ do
      let optionalMacro =
            let mName = Identifier "optional-add-two-to-five"
                mRegularParams = Identifier <$> ["a", "b"]
                mOptionalParams = Identifier <$> ["c", "d", "e"]
                mRestParam = Nothing
                mResult = FAL
                  [ FAId_ "+"
                  , FAId_ "a"
                  , FAId_ "b"
                  , FAId_ "c"
                  , FAId_ "d"
                  , FAId_ "e"
                  ]
            in  Macro mName mRegularParams mOptionalParams mRestParam mResult

      toMacro
        (FAL
         [ FAId_ "defmacro"
         , FAId_ "optional-add-two-to-five"
         , FAL $ FAId_ <$> ["a", "b", "&optional", "c", "d", "e"]
         , FAL
           [ FAId_ "+"
           , FAId_ "a"
           , FAId_ "b"
           , FAId_ "c"
           , FAId_ "d"
           , FAId_ "e"
           ]
         ]) `shouldBe` Just optionalMacro

    it "parses a defmacro macro declaration with an &rest argument" $ do
      let restMacro =
            let mName = Identifier "rest-add-two-or-more"
                mRegularParams = Identifier <$> ["a", "b"]
                mOptionalParams = []
                mRestParam = Just . Identifier $ "everything-else"
                mResult = FAL
                  [ FAId_ "+"
                  , FAId_ "a"
                  , FAId_ "b"
                  , FABQ . Spliced $ FAId_ "everything-else"
                  ]
            in Macro mName mRegularParams mOptionalParams mRestParam mResult
      toMacro
        (FAL
         [ FAId_ "defmacro"
         , FAId_ "rest-add-two-or-more"
         , FAL $ FAId_ <$> ["a", "b", "&rest", "everything-else"]
         , FAL
           [ FAId_ "+"
           , FAId_ "a"
           , FAId_ "b"
           , FABQ . Spliced $ FAId_ "everything-else"
           ]
         ]) `shouldBe` Just restMacro

    it "parses a defmacro macro declaration with optional arguments \
       \and an &rest argument" $ do
      let optionalAndRestMacro =
            let mName = Identifier "optional-rest-add-two-or-more"
                mRegularParams = []
                mOptionalParams = Identifier <$> ["a", "b"]
                mRestParam = Just . Identifier $ "everything-else"
                mResult = FAL
                  [ FAId_ "+"
                  , FAId_ "a"
                  , FAId_ "b"
                  , FABQ . Spliced $ FAId_ "everything-else"
                  ]
            in Macro mName mRegularParams mOptionalParams mRestParam mResult
      toMacro
        (FAL
         [ FAId_ "defmacro"
         , FAId_ "optional-rest-add-two-or-more"
         , FAL $ FAId_
           <$> ["&optional", "a", "b", "&rest", "everything-else"]
         , FAL
           [ FAId_ "+"
           , FAId_ "a"
           , FAId_ "b"
           , FABQ . Spliced $ FAId_ "everything-else"
           ]
         ]) `shouldBe` Just optionalAndRestMacro

  describe "macroExpandWith" $ do
      it "has no effect on expressions that don't contain a macro" $ do
          macroExpandWith macros (FAIn 1)
              `shouldBe` Just (FAIn 1)
      it "leaves unrelated identifiers alone" $ do
          macroExpandWith macros (FAL [FAId_ "goo", FAL [FAId_ "bar"]])
              `shouldBe` Just (FAL [FAId_ "goo", FAIn 0])
      it "expands macros" $ do
          macroExpandWith macros (FAL [FAL [FAId_ "foo", FAIn 2, FAId_ "a"]])
              `shouldBe` Just (FAL [FAL [FAId_ "+", FAId_ "a", FAIn 2]])

  describe "macroExpand" $ do
      let original = (FAL [
              FAL [
                  FAId_ "defmacro"
                , FAId_ "foo"
                , FAL [FAId_ "x", FAId_ "y"]
                , FAL [FAId_ "+", FAId_ "y", FAId_ "x"]
                ]
            , FAL [
                  FAId_ "foo"
                , FAL [FAId_ "a"]
                , FAL [FAId_ "b"]
                ]
            ])
          expected = (FAL [
              FAL [
                  FAId_ "defmacro"
                , FAId_ "foo"
                , FAL [FAId_ "x", FAId_ "y"]
                , FAL [FAId_ "+", FAId_ "y", FAId_ "x"]
                ]
            , FAL [
                  FAId_ "+"
                , FAL [FAId_ "b"]
                , FAL [FAId_ "a"]
                ]
            ])
      it "locates and expands macros" $ do
          macroExpand original `shouldBe` Just expected

      it "shouldn't find nested macro definitions" $ do
          macroExpand (FAL [original]) `shouldBe` Just (FAL [original])

      it "fully expands expressions requiring repeated expansion" $ do
          let original = FAL [
                  FAL [
                      FAId_ "defmacro"
                    , FAId_ "f"
                    , FAL [FAId_ "n"]
                    , FAL [
                          FAId_ "*"
                        , FAId_ "n"
                        , FAL [
                              FAId_ "g"
                            , FAId_ "n"
                            ]
                        ]
                    ]
                , FAL [
                      FAId_ "defmacro"
                    , FAId_ "g"
                    , FAL [FAId_ "n"]
                    , FAL [
                          FAId_ "+"
                        , FAId_ "n"
                        , FAId_ "1"
                        ]
                    ]
                , FAL [
                      FAId_ "f"
                    , FAId_ "1"
                    ]
                ]
          let expected = FAL [
                  FAL [
                      FAId_ "defmacro"
                    , FAId_ "f"
                    , FAL [FAId_ "n"]
                    , FAL [
                          FAId_ "*"
                        , FAId_ "n"
                        , FAL [
                              FAId_ "g"
                            , FAId_ "n"
                            ]
                        ]
                    ]
                , FAL [
                      FAId_ "defmacro"
                    , FAId_ "g"
                    , FAL [FAId_ "n"]
                    , FAL [
                          FAId_ "+"
                        , FAId_ "n"
                        , FAId_ "1"
                        ]
                    ]
                , FAL [
                      FAId_ "*"
                    , FAId_ "1"
                    , FAL [
                          FAId_ "+"
                        , FAId_ "1"
                        , FAId_ "1"
                        ]
                    ]
                ]
          macroExpand original `shouldBe` Just expected
