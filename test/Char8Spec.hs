module Char8Spec where

import qualified Data.Char as C
import Data.Char8
import Test.Hspec
import Test.Hspec.QuickCheck

spec :: Spec
spec = do
    describe "isControl" $ do
        prop "behaves like model" $ \c ->
            isControl c == C.isControl c

    describe "isSpace" $ do
        prop "behaves like model" $ \c ->
            isSpace c == C.isSpace c

    describe "isLower" $ do
        prop "behaves like model" $ \c ->
            isLower c == C.isLower c

    describe "isUpper" $ do
        prop "behaves like model" $ \c ->
            isUpper c == C.isUpper c

    describe "isAlpha" $ do
        prop "behaves like model" $ \c ->
            isAlpha c == C.isAlpha c

    describe "isAlphaNum" $ do
        prop "behaves like model" $ \c ->
            isAlphaNum c == C.isAlphaNum c

    describe "isPrint" $ do
        prop "behaves like model" $ \c ->
            isPrint c == C.isPrint c

    describe "isDigit" $ do
        prop "behaves like model" $ \c ->
            isDigit c == C.isDigit c

    describe "isOctDigit" $ do
        prop "behaves like model" $ \c ->
            isOctDigit c == C.isOctDigit c

    describe "isHexDigit" $ do
        prop "behaves like model" $ \c ->
            isHexDigit c == C.isHexDigit c

    describe "isLetter" $ do
        prop "behaves like model" $ \c ->
            isLetter c == C.isLetter c

    describe "isMark" $ do
        prop "behaves like model" $ \c ->
            isMark c == C.isMark c

    describe "isNumber" $ do
        prop "behaves like model" $ \c ->
            isNumber c == C.isNumber c

    describe "isPunctuation" $ do
        prop "behaves like model" $ \c ->
            isPunctuation c == C.isPunctuation c

    describe "isSymbol" $ do
        prop "behaves like model" $ \c ->
            isSymbol c == C.isSymbol c

    describe "isSeparator" $ do
        prop "behaves like model" $ \c ->
            isSeparator c == C.isSeparator c

    describe "isAscii" $ do
        prop "behaves like model" $ \c ->
            isAscii c == C.isAscii c

    describe "isLatin1" $ do
        prop "behaves like model" $ \c ->
            isLatin1 c == C.isLatin1 c

    describe "isAsciiUpper" $ do
        prop "behaves like model" $ \c ->
            isAsciiUpper c == C.isAsciiUpper c

    describe "isAsciiLower" $ do
        prop "behaves like model" $ \c ->
            isAsciiLower c == C.isAsciiLower c

    describe "toUpper" $ do
        prop "behaves like model" $ prop_toUpper

    describe "toLower" $ do
        prop "behaves like model" $ \c ->
            toLower c == C.toLower c

    describe "toTitle" $ do
        prop "behaves like model" $ prop_toTitle

prop_toUpper :: Char -> Bool
prop_toUpper c
  | c == _mu        = True
  | c == _ydieresis = True
  | otherwise       = toUpper c == C.toUpper c
    
prop_toTitle :: Char -> Bool
prop_toTitle c
  | c == _mu  = True
  | c == _ydieresis = True
  | otherwise = toTitle c == C.toTitle c

----------------------------------------------------------------

_mu, _ydieresis :: Char
_mu        = '\xb5'
_ydieresis = '\xff'
