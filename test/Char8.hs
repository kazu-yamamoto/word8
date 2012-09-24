{-# LANGUAGE TemplateHaskell #-}

module Test where

import qualified Data.Char as C
import Data.Char8
import Test.Framework.Providers.QuickCheck2
import Test.Framework.TH.Prime

prop_isControl :: Char -> Bool
prop_isControl c = isControl c == C.isControl c

prop_isSpace :: Char -> Bool
prop_isSpace c = isSpace c == C.isSpace c

prop_isLower :: Char -> Bool
prop_isLower c = isLower c == C.isLower c

prop_isUpper :: Char -> Bool
prop_isUpper c = isUpper c == C.isUpper c

prop_isAlpha :: Char -> Bool
prop_isAlpha c = isAlpha c == C.isAlpha c

prop_isAlphaNum :: Char -> Bool
prop_isAlphaNum c = isAlphaNum c == C.isAlphaNum c

prop_isPrint :: Char -> Bool
prop_isPrint c = isPrint c == C.isPrint c

prop_isDigit :: Char -> Bool
prop_isDigit c = isDigit c == C.isDigit c

prop_isOctDigit :: Char -> Bool
prop_isOctDigit c = isOctDigit c == C.isOctDigit c

prop_isHexDigit :: Char -> Bool
prop_isHexDigit c = isHexDigit c == C.isHexDigit c

prop_isLetter :: Char -> Bool
prop_isLetter c = isLetter c == C.isLetter c

prop_isMark :: Char -> Bool
prop_isMark c = isMark c == C.isMark c

prop_isNumber :: Char -> Bool
prop_isNumber c = isNumber c == C.isNumber c

prop_isPunctuation :: Char -> Bool
prop_isPunctuation c = isPunctuation c == C.isPunctuation c

prop_isSymbol :: Char -> Bool
prop_isSymbol c = isSymbol c == C.isSymbol c

prop_isSeparator :: Char -> Bool
prop_isSeparator c = isSeparator c == C.isSeparator c

prop_isAscii :: Char -> Bool
prop_isAscii c = isAscii c == C.isAscii c

prop_isLatin1 :: Char -> Bool
prop_isLatin1 c = isLatin1 c == C.isLatin1 c

prop_isAsciiUpper :: Char -> Bool
prop_isAsciiUpper c = isAsciiUpper c == C.isAsciiUpper c

prop_isAsciiLower :: Char -> Bool
prop_isAsciiLower c = isAsciiLower c == C.isAsciiLower c

prop_toUpper :: Char -> Bool
prop_toUpper c
  | c == _mu        = True
  | c == _ydieresis = True
  | otherwise       = toUpper c == C.toUpper c

prop_toLower :: Char -> Bool
prop_toLower c = toLower c == C.toLower c

prop_toTitle :: Char -> Bool
prop_toTitle c
  | c == _mu  = True
  | c == _ydieresis = True
  | otherwise = toTitle c == C.toTitle c

----------------------------------------------------------------

_mu, _ydieresis :: Char
_mu        = '\xb5'
_ydieresis = '\xff'

main :: IO ()
main = $(defaultMainGenerator)
