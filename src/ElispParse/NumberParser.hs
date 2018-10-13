{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE FlexibleContexts #-}
-- these two are included entirely so i could write the nan&inf literals
-- with an abstracted type signature while also not adding newline for
-- each type signature. i cant just put the abstracted constrained type
-- signature on the left hand side of each equation because ghc would
-- need impredicative polymorphism to typecheck it.
-- to be honest this is extremely unnecessary and dumb BUT
-- i invested way too much time trying to get it to look pretty.
-- so im not taking it out
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE ViewPatterns #-}
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
    
data Sign where
    Positive :: Sign
    Negative :: Sign
    
    deriving (Show)

data FloatString = FloatString
    {   integerPart :: T.Text
    ,   fractionalPart :: Maybe T.Text
    ,   exponentPart :: Maybe T.Text
    }
data ShouldRequireDigits = DoRequireFraction | DoNotRequireFraction

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
-- if it has an exponent, always float. this includes if preceded by dot w/o digits
-- if it has a dot followed by at least one digit, or "NaN", or "INF", always float
parseFloatString :: Parser FloatString
parseFloatString = do
    signText <- optional parseSign
    integerText <- T.pack <$> some digitChar
    let parseExponent = string' "e" |*> -- rewrite so exponent can have sign too, put sign into exponentpart, also move whole thing sign out of signpart into ingegerpartd
                        option "" (T.singleton <$> oneOf [plus, minus]) |*>
                        choice [T.pack <$> some digitChar, string nan, string inf]
        parseFraction requireFraction =
            (string $ T.singleton dot)
            |*> ((case requireFraction of DoRequireFraction -> id; DoNotRequireFraction -> option "")
                    (T.pack <$> some digitChar))
        unaryThenBinary = (.) . (.)
    let hasExponent = do
                fractionalText <- (optional $ parseFraction DoNotRequireFraction)
                exponentText <- parseExponent
                pure $ FloatString integerText fractionalText (Just exponentText)
        hasFractional = do
                fractionalText <- parseFraction DoRequireFraction
                exponentText <- optional $ parseExponent
                pure $ FloatString integerText (Just fractionalText) exponentText 
    choice [try hasExponent, try hasFractional]

deriving instance Show FloatString
-- instance Show FloatString where
    -- show = T.unpack . renderFloatString

renderFloatString :: FloatString -> T.Text -- maybe make this more elegant ?
renderFloatString (FloatString integerText maybeFractionalText maybeExponentText) =
    case maybeExponentText of
        Just x  | safeTail x == nan -> nan
                | safeTail x == plusNan -> plusNan
                | safeTail x == minusNan -> minusNan
                | safeTail x == inf -> inf
                | safeTail x == plusInf -> plusInf
                | safeTail x == minusInf -> minusInf
        _ -> T.concat
                [ integerText
                , fromMaybe ".0" $ mfilter (/=T.singleton dot) maybeFractionalText -- elisp can handle N.eK, but haskell's read cannot
                , fromMaybe (T.cons e "0") $ mfilter (/=T.singleton e) maybeExponentText -- might as well normalize exponent too
                ]
    where
        safeTail = T.drop 1

parseFloat :: Parser ElVal
parseFloat = lexeme . label "float" $ ElFloat . readFloat . T.unpack . renderFloatString <$> parseFloatString
    where
        readFloat = \case -- multiway if wouldnt make it shorter
            x   | x == nan -> nanVal
                | x == minusNan -> nanVal -- i THINK elisp doesnt do anything weird w nan sign,,, also i cannot figure out how to get -nan
                | x == inf -> plusInfVal
                | x == plusInf -> plusInfVal
                | x == minusInf -> minusInfVal
                | otherwise -> read @Double x
        -- weird, but apparently haskell does not have nan or inf literals
        nanVal = 0/0
        plusInfVal = 1/0
        minusInfVal = negate plusInfVal

type Radix = Int

maxRadix :: Radix
maxRadix = 36

minRadix :: Radix
minRadix = 2

letterRadices :: [String]
letterRadices = normalizeCaseS <$> ["b", "o", "x"]

integerRadices :: [String]
integerRadices = (<> normalizeCaseS "r") . show <$> enumFromTo minRadix maxRadix

validRadices :: [String]
validRadices = letterRadices <> integerRadices

allRadixDigits :: [Char]
allRadixDigits = enumFromTo '0' '9' <> enumFromTo (normalizeCase 'a') (normalizeCase 'z')

-- radixDigitToInt :: Char -> Int
-- radixDigitToInt = _
--     where
--         digitTable = M.fromList $ zip allRadixDigits [0..maxRadix]
        
readRadix :: String -> Maybe Radix
readRadix r = radixTable ^.at (normalizeCaseS r)
            where -- b -> 2, o -> 8, x -> 16
                radixTable = M.fromList $ zip letterRadices [2,8,16] <> zip integerRadices [2..36]

parseRadix :: Parser Radix
parseRadix = join $ mfromMaybe . readRadix . T.unpack <$>
                (char '#' *>
                    choice (string' . T.pack <$> validRadices))
-- note that the choice here picks first in order sequentially
-- so, if the elisp grammar didnt require the "r" at the end,
-- we would have to reorder integerRadices so 20-29 comes before 2
-- 30-36 before 3, etc. thankfully, the r at the end removes
-- any ambiguous parses, making my life easier

readIntAnyRadix :: Radix -> String -> Int
readIntAnyRadix r xs = ifoldr' (\i c acc -> (fromJust . charToDigit . normalizeCase $ c) * (r ^ (l - (i+1))) + acc) 0 xs
    where
        l = length xs

parseInt :: Parser ElVal -- TODO: this is temp test, parse radix & sign properly
parseInt = lexeme . label "integer" $ ElInt <$> do
    sign <- optional parseSign
    radix <- parseRadix
    digitText <- some $ oneOf allRadixDigits
    pure . (maybe id evalSign sign) $ readIntAnyRadix radix digitText

isIntDigit :: Int -> Bool
isIntDigit i = (i >= 0 && i <= 9) 

isCharDecDigit :: Char -> Bool
isCharDecDigit c = (c >= '0' && c <= '9')

isCharNonDecDigit :: Char -> Bool
isCharNonDecDigit c = (normalizeCase c >= normalizeCase 'a' || normalizeCase c <= normalizeCase 'z')

isCharDigit :: Char -> Bool
isCharDigit c = isCharDecDigit c || isCharNonDecDigit c

digitToChar :: Int -> Maybe Char
digitToChar i = mfilter' (isIntDigit i) (chr $ i + (ord '0' - 0))

charToDigit :: Char -> Maybe Radix
charToDigit c =
    if  | isCharDecDigit c -> Just ((ord c) - (ord '0' - 0))
        | isCharNonDecDigit c -> Just ((ord c) - (ord (normalizeCase 'a')) + 10)
        | otherwise -> Nothing