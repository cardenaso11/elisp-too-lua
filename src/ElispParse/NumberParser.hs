{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE FlexibleContexts #-}
-- these two are included entirely so i could write the nan&inf literals
-- with an abstracted type signature while also not adding newline for
-- each type signature. i cant just put the abstracted constrained type
-- signature on the left hand side of each equation because ghc would
-- need impredicative polymorphism to typecheck it.
-- to be honest this is unnecessary and kinda lame BUT
-- i invested way too much time trying to get it to look pretty.
-- so im not taking it out
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiWayIf #-}

module ElispParse.NumberParser
    ( parseFloat
    , parseInt) where

import GHC.Generics
import qualified Data.Text as T
import Data.Hashable
import qualified Data.HashMap.Strict as HM
import Control.Monad
import Control.Lens
import Data.Monoid
import Data.Maybe
import Data.Char
import Data.String
import qualified Data.Map.Strict as M
import qualified Data.Functor.Identity
import Data.Void
import Data.Proxy
import Text.Megaparsec as M
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L

import ElispParse.Common

import Debug.Trace

-- | A Sign is either Positive or Negative.
data Sign where
    Positive :: Sign
    Negative :: Sign

    deriving (Show)

-- | A float has an integer part and optional fractional and exponent parts.
data FloatString = FloatString
    {   integerPart :: T.Text
    ,   fractionalPart :: Maybe T.Text
    ,   exponentPart :: Maybe T.Text
    }

data ShouldRequireDigits = DoRequireFraction | DoNotRequireFraction

-- | Type for generic strings.
type PString s = (IsString s) => s

nan = "NaN" :: PString s
inf = "INF" :: PString s
plus = '+' :: Char
minus = '-' :: Char
dot = '.' :: Char
e = 'e' :: Char
plusNan = fromString $ plus : nan :: PString s
minusNan = fromString $ minus : nan :: PString s
plusInf = fromString $ plus : inf :: PString s
minusInf = fromString $ minus : inf :: PString s

parseSign :: Parser Sign
parseSign = oneOf [plus, minus] >>= \x ->
    if  | x == plus -> pure Positive
        | x == minus -> pure Negative
        | otherwise -> mzero

evalSign :: Sign -> Int -> Int
evalSign = \case
    Positive -> abs
    Negative -> negate . abs

-- exactly two rules here:
-- if it has an exponent, float. this includes if preceded by dot w/o digits
-- if it has a dot followed by at least 1 digit, or "NaN", or "INF", float
parseFloatString :: Parser FloatString
parseFloatString = do
    signText <- optional parseSign
    integerText <- option "0" $ T.pack <$> some digitChar
    let parseExponent =
          string' "e"
          <> option "" (T.singleton <$> oneOf [plus, minus])
          <> choice [T.pack <$> some digitChar, string nan, string inf]
        parseFraction requireFraction =
            string (T.singleton dot)
            <> (case requireFraction of
                   DoRequireFraction -> id
                   DoNotRequireFraction -> option "")
            (T.pack <$> some digitChar)
    let hasExponent =
          do
            fractionalText <- optional $ parseFraction DoNotRequireFraction
            exponentText <- parseExponent
            pure $ FloatString integerText fractionalText (Just exponentText)
        hasFractional =
          do
            fractionalText <- parseFraction DoRequireFraction
            exponentText <- optional parseExponent
            pure $ FloatString integerText (Just fractionalText) exponentText
    choice [try hasExponent, try hasFractional]

deriving instance Show FloatString

renderFloatString :: FloatString -> T.Text -- maybe make this more elegant ?
renderFloatString
  (FloatString integerText maybeFractionalText maybeExponentText) =
    case maybeExponentText of
      Just x  | safeTail x == nan -> nan
              | safeTail x == plusNan -> plusNan
              | safeTail x == minusNan -> minusNan
              | safeTail x == inf -> inf
              | safeTail x == plusInf -> plusInf
              | safeTail x == minusInf -> minusInf
      _ -> T.concat
           [ integerText
           , fromMaybe ".0"
             $ mfilter (/=T.singleton dot) maybeFractionalText
           -- elisp can handle N.eK, but haskell's read cannot
           , fromMaybe (T.cons e "0")
             $ mfilter (/=T.singleton e) maybeExponentText
           -- might as well normalize exponent too
           ]
  where
    safeTail = T.drop 1

-- | Parse an emacs floating point number.
parseFloat :: BaseParser a
parseFloat = lexeme . label "float" $
  ASTFloat . readFloat . T.unpack . renderFloatString
  <$> parseFloatString
    where
        readFloat = \case -- multiway if wouldnt make it shorter
            x   | x == nan -> nanVal
                | x == minusNan -> nanVal
                -- i THINK elisp doesnt do anything weird w nan sign,,,
                -- also i cannot figure out how to get -nan
                | x == inf -> plusInfVal
                | x == plusInf -> plusInfVal
                | x == minusInf -> minusInfVal
                | otherwise -> read @Double x
        -- weird, but haskell does not have nan or inf literals
        nanVal = 0/0
        plusInfVal = 1/0
        minusInfVal = negate plusInfVal

type Radix = Int

-- | Maximum radix supported by emacs.
maxRadix :: Radix
maxRadix = 36

-- | Minimum radix.
minRadix :: Radix
minRadix = 2

-- | Special binary, octal, and hex radix symbols.
letterRadices :: [String]
letterRadices = normalizeCaseS <$> ["b", "o", "x"]

-- | Radix symbols for radices from 2 through 36.
integerRadices :: [String]
integerRadices = (<> normalizeCaseS "r") . show
  <$> enumFromTo minRadix maxRadix

-- | All valid radices, including the special 'b', 'o' and 'x' radices.
validRadices :: [String]
validRadices = letterRadices <> integerRadices

-- | Set of all digits that can appear in numbers of different radices.
allRadixDigits :: [Char]
allRadixDigits = enumFromTo '0' '9'
  <> enumFromTo (normalizeCase 'a') (normalizeCase 'z')

-- | Read a string as a radix of the form 'b', 'o', 'x', or 'rN' for some number N.
readRadix :: String -> Maybe Radix
readRadix r = radixTable ^.at (normalizeCaseS r)
  where -- b -> 2, o -> 8, x -> 16
    radixTable =
      M.fromList $ zip letterRadices [2,8,16] <> zip integerRadices [2..36]

-- | Parse a radix.
parseRadix :: Parser Radix
parseRadix = join $ mfromMaybe . readRadix . T.unpack <$>
                (char '#' *>
                    choice (string' . T.pack <$> validRadices))
-- note that the choice here picks first in order sequentially
-- so, if the elisp grammar didnt require the "r" at the end,
-- we would have to reorder integerRadices so 20-29 comes before 2
-- 30-36 before 3, etc. thankfully, the r at the end removes
-- any ambiguous parses, making my life easier

-- | Given a radix and a string representation of a number, read it as an integer.
readIntAnyRadix :: Radix -> String -> Int
readIntAnyRadix r xs =
  ifoldr' (\i c acc ->
             (fromJust . charToDigit . normalizeCase $ c)
             * (r ^ (l - (i+1))) + acc)
  0 xs
    where
        l = length xs

-- | Parse an emacs integer.
parseInt :: BaseParser a
parseInt = lexeme . label "integer" $ ASTInt <$> do
    sign <- optional parseSign
    radix <- let defaultRadix = 10 in
        fromMaybe defaultRadix <$> optional parseRadix
    digitText <- let allowedDigits = take radix allRadixDigits in
        some $ oneOf allowedDigits
    pure . maybe id evalSign sign $ readIntAnyRadix radix digitText

-- | Indicate whether a character is a decimal digit.
isCharDecDigit :: Char -> Bool
isCharDecDigit c = c >= '0' && c <= '9'

-- | Indicate whether a character is a non-decimal digit (i.e., an uppercase or lowercase letter).
isCharNonDecDigit :: Char -> Bool
isCharNonDecDigit c =
  normalizeCase c >= normalizeCase 'a'
  || normalizeCase c <= normalizeCase 'z'

-- | Convert a digit character to the integer value it represents, or Nothing if not a valid digit.
charToDigit :: Char -> Maybe Radix
charToDigit c =
    if  | isCharDecDigit c -> Just (ord c - (ord '0' - 0))
        | isCharNonDecDigit c -> Just (ord c - ord (normalizeCase 'a') + 10)
        | otherwise -> Nothing