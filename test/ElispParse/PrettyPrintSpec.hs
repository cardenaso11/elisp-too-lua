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

shouldRender :: T.Text -> Expectation
shouldRender ex =
    let render = renderLazy . layoutPretty defaultLayoutOptions . pretty
    in  fmap render (parseText exprFP ex) `shouldBe` Right (render ex)
