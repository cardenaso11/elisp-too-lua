{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE OverloadedStrings #-}

module ElispParse.PrettyPrintSpec
    ( spec
    )
where

import           Test.Hspec
import qualified Data.Text.Lazy as T
import           Data.Text.Prettyprint.Doc
import           Data.Text.Prettyprint.Doc.Render.Text

import ElispParse.TestCommon
import ElispParse.Common
import ElispParse.ElispParser
import ElispParse.PrettyPrint

spec = do
    describe "pretty" $ do
        it "produces pretty backquoted lists" $ do
            let ex = "`(1 ,2 3)"
            shouldRender ex
        it "produces pretty function definitions" $ do
            let func = "(defun sq (n &optional foo &rest bar) (* n n))"
            shouldRender func
        it "handles vectors" $ do
            let ex = "[1 2 3]"
            shouldRender ex
        it "handles chars" $ do
            let char = "?a"
            shouldRender char
            show (pretty (FASTChar 'a')) `shouldBe` "?a"
        it "handles strings" $ do
            show (pretty (FASTString "awoo")) `shouldBe` "\"awoo\""
        it "escapes strings" $ do
            show (pretty (FASTString "\"awoo\"")) `shouldBe` "\"\\\"awoo\\\"\""
        it "handles cons cells" $ do
            show (pretty (FASTCons [FASTInt 1] (FASTInt 2))) `shouldBe` "(1 . 2)"
        it "handles floats" $ do
            show (pretty (FASTFloat 2.71)) `shouldBe` "2.71"
    describe "program" $ do
        it "recognizes ASTs that represent programs" $ do
            let code = "(defun foo () 1)\n(defun bar () 2)"
            let ast =
                    FAL
                      [ FAL [ FAId_ "defun", FAId_ "foo", FAL [], FAIn 1 ]
                      , FAL [ FAId_ "defun", FAId_ "bar", FAL [], FAIn 2 ]
                      ]
            fmap show (program ast) `shouldBe` Just code

shouldRender :: T.Text -> Expectation
shouldRender ex =
    let render = renderLazy . layoutPretty defaultLayoutOptions . pretty
    in  fmap render (parseText exprFP ex) `shouldBe` Right ex
