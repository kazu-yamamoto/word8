{-# LANGUAGE TemplateHaskell #-}

module Test where

import qualified Data.Char as C
import Data.Word (Word8)
import Data.Word8
import Test.Framework.Providers.QuickCheck2
import Test.Framework.TH.Prime

prop_isControl :: Word8 -> Bool
prop_isControl w = isControl w == C.isControl (word8ToChar w)

prop_isSpace :: Word8 -> Bool
prop_isSpace w = isSpace w == C.isSpace (word8ToChar w)

prop_isLower :: Word8 -> Bool
prop_isLower w = isLower w == C.isLower (word8ToChar w)

prop_isUpper :: Word8 -> Bool
prop_isUpper w = isUpper w == C.isUpper (word8ToChar w)

prop_isAlpha :: Word8 -> Bool
prop_isAlpha w = isAlpha w == C.isAlpha (word8ToChar w)

prop_isAlphaNum :: Word8 -> Bool
prop_isAlphaNum w = isAlphaNum w == C.isAlphaNum (word8ToChar w)

prop_isPrint :: Word8 -> Bool
prop_isPrint w = isPrint w == C.isPrint (word8ToChar w)

prop_isDigit :: Word8 -> Bool
prop_isDigit w = isDigit w == C.isDigit (word8ToChar w)

prop_isOctDigit :: Word8 -> Bool
prop_isOctDigit w = isOctDigit w == C.isOctDigit (word8ToChar w)

prop_isHexDigit :: Word8 -> Bool
prop_isHexDigit w = isHexDigit w == C.isHexDigit (word8ToChar w)

prop_isLetter :: Word8 -> Bool
prop_isLetter w = isLetter w == C.isLetter (word8ToChar w)

prop_isMark :: Word8 -> Bool
prop_isMark w = isMark w == C.isMark (word8ToChar w)

prop_isNumber :: Word8 -> Bool
prop_isNumber w = isNumber w == C.isNumber (word8ToChar w)

prop_isPunctuation :: Word8 -> Bool
prop_isPunctuation w = isPunctuation w == C.isPunctuation (word8ToChar w)

prop_isSymbol :: Word8 -> Bool
prop_isSymbol w = isSymbol w == C.isSymbol (word8ToChar w)

prop_isSeparator :: Word8 -> Bool
prop_isSeparator w = isSeparator w == C.isSeparator (word8ToChar w)

prop_isAscii :: Word8 -> Bool
prop_isAscii w = isAscii w == C.isAscii (word8ToChar w)

prop_isLatin1 :: Word8 -> Bool
prop_isLatin1 w = isLatin1 w == C.isLatin1 (word8ToChar w)

prop_isAsciiUpper :: Word8 -> Bool
prop_isAsciiUpper w = isAsciiUpper w == C.isAsciiUpper (word8ToChar w)

prop_isAsciiLower :: Word8 -> Bool
prop_isAsciiLower w = isAsciiLower w == C.isAsciiLower (word8ToChar w)

prop_toUpper :: Word8 -> Bool
prop_toUpper w
  | w == _mu        = True
  | w == _ydieresis = True
  | otherwise       = word8ToChar (toUpper w) == C.toUpper (word8ToChar w)

prop_toLower :: Word8 -> Bool
prop_toLower w = word8ToChar (toLower w) == C.toLower (word8ToChar w)

prop_toTitle :: Word8 -> Bool
prop_toTitle w
  | w == _mu  = True
  | w == _ydieresis = True
  | otherwise = word8ToChar (toTitle w) == C.toTitle (word8ToChar w)

----------------------------------------------------------------

main :: IO ()
main = $(defaultMainGenerator)

word8ToChar :: Word8 -> Char
word8ToChar = C.chr . fromIntegral
