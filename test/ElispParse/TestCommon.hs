{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE PatternSynonyms #-}

module ElispParse.TestCommon
  ( shouldParse'
  , pattern FAL
  , pattern FAQ
  , pattern FABQ
  , pattern FAV
  , pattern FAT
  , pattern FACo
  , pattern FACT
  , pattern FACTST
  , pattern FABC
  , pattern FAId
  , pattern FAId_
  , pattern FAF
  , pattern FAIn
  , pattern FACh
  , pattern FAS
  , pattern FABV
  , pattern AId_
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

-- abbreviated pattern synonyms to make tests legible
pattern FAL x = FASTList x
pattern FAQ x = FASTQuote x
pattern FABQ x = FASTBackquote x
pattern FAV x = FASTVector x
pattern FAT x = FASTTable x
pattern FACo x xs = FASTCons x xs
pattern FACT x = FASTCharTable x
pattern FACTST x = FASTCharSubTable x
pattern FABC x = FASTByteCode x
pattern FAId x = FASTIdentifier x
pattern FAId_ x = FASTIdentifier_ x
pattern FAF x = FASTFloat x
pattern FAIn x = FASTInt x
pattern FACh x = FASTChar x
pattern FAS x = FASTString x
pattern FABV x y = FASTBoolVector x y

pattern AId_ x = ASTIdentifier_ x
