{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

module ElispParse.TestCommon
  ( shouldParse'
  ) where

import Test.Hspec
import Test.Hspec.Megaparsec
import Text.Megaparsec as M
import Text.Megaparsec.Error
import Text.Megaparsec.Char
import Data.Void
import qualified Data.Text.Lazy as T

import ElispParse.Common
import ElispParse.NumberParser

shouldParse' :: Either (ParseError Char Void) InfiniteAST -> InfiniteAST -> Expectation
shouldParse' = shouldParse
