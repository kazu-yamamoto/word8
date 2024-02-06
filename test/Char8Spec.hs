{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralisedNewtypeDeriving #-}

module Char8Spec where

import qualified Data.Char as C
import Data.Char8
import Data.Word8 (Word8)
import Test.Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck (Arbitrary (..), withMaxSuccess)

allChar8 :: String
allChar8 = ['\0' .. '\255']

spec :: Spec
spec = do
    describe "isControl" $ do
        it "behaves like model" $
            (isControl <$> allChar8)
                == (C.isControl <$> allChar8)

    describe "isSpace" $ do
        it "behaves like model" $
            (isSpace <$> allChar8)
                == (C.isSpace <$> allChar8)

    describe "isLower" $ do
        it "behaves like model" $
            (isLower <$> allChar8)
                == (C.isLower <$> allChar8)

    describe "isUpper" $ do
        it "behaves like model" $
            (isUpper <$> allChar8)
                == (C.isUpper <$> allChar8)

    describe "isAlpha" $ do
        it "behaves like model" $
            (isAlpha <$> allChar8)
                == (C.isAlpha <$> allChar8)

    describe "isAlphaNum" $ do
        it "behaves like model" $
            (isAlphaNum <$> allChar8)
                == (C.isAlphaNum <$> allChar8)

    describe "isPrint" $ do
        it "behaves like model" $
            (isPrint <$> allChar8)
                == (C.isPrint <$> allChar8)

    describe "isDigit" $ do
        it "behaves like model" $
            (isDigit <$> allChar8)
                == (C.isDigit <$> allChar8)

    describe "isOctDigit" $ do
        it "behaves like model" $
            (isOctDigit <$> allChar8)
                == (C.isOctDigit <$> allChar8)

    describe "isHexDigit" $ do
        it "behaves like model" $
            (isHexDigit <$> allChar8)
                == (C.isHexDigit <$> allChar8)

    describe "isLetter" $ do
        it "behaves like model" $
            (isLetter <$> allChar8)
                == (C.isLetter <$> allChar8)

    describe "isMark" $ do
        it "behaves like model" $
            (isMark <$> allChar8)
                == (C.isMark <$> allChar8)

    describe "isNumber" $ do
        it "behaves like model" $
            (isNumber <$> allChar8)
                == (C.isNumber <$> allChar8)

    describe "isPunctuation" $ do
        it "behaves like model" $
            (isPunctuation <$> allChar8)
                == (C.isPunctuation <$> allChar8)

    describe "isSymbol" $ do
        it "behaves like model" $
            (isSymbol <$> allChar8)
                == (C.isSymbol <$> allChar8)

    describe "isSeparator" $ do
        it "behaves like model" $
            (isSeparator <$> allChar8)
                == (C.isSeparator <$> allChar8)

    describe "isAscii" $ do
        it "behaves like model" $
            (isAscii <$> allChar8)
                == (C.isAscii <$> allChar8)

    describe "isLatin1" $ do
        it "behaves like model" $
            (isLatin1 <$> allChar8)
                == (C.isLatin1 <$> allChar8)

    describe "isAsciiUpper" $ do
        it "behaves like model" $
            (isAsciiUpper <$> allChar8)
                == (C.isAsciiUpper <$> allChar8)

    describe "isAsciiLower" $ do
        it "behaves like model" $
            (isAsciiLower <$> allChar8)
                == (C.isAsciiLower <$> allChar8)

    describe "toUpper" $ do
        prop "behaves like model" $
            withMaxSuccess 10000 prop_toUpper

    describe "toLower" $ do
        it "behaves like model" $
            (toLower <$> allChar8)
                == (C.toLower <$> allChar8)

    describe "toTitle" $ do
        prop "behaves like model" $
            withMaxSuccess 10000 prop_toTitle

prop_toUpper :: Char8 -> Bool
prop_toUpper (Char8 c)
    | c == _mu = True
    | c == _ydieresis = True
    | otherwise = toUpper c == C.toUpper c

prop_toTitle :: Char8 -> Bool
prop_toTitle (Char8 c)
    | c == _mu = True
    | c == _ydieresis = True
    | otherwise = toTitle c == C.toTitle c

----------------------------------------------------------------

_mu, _ydieresis :: Char
_mu = '\xb5'
_ydieresis = '\xff'

newtype Char8 = Char8 Char
    deriving newtype (Eq, Show)

instance Arbitrary Char8 where
    arbitrary = do
        w8 <- arbitrary
        pure $ Char8 $ C.chr $ fromIntegral (w8 :: Word8)
