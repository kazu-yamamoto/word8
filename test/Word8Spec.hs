module Word8Spec where

import qualified Data.Char as C
import Data.Word8
import Test.Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck (withMaxSuccess)

allWord8 :: [Word8]
allWord8 = [0 .. 255]

spec :: Spec
spec = do
    describe "isControl" $ do
        it "behaves like model" $
            (isControl <$> allWord8)
                == (C.isControl . word8ToChar <$> allWord8)

    describe "isSpace" $ do
        it "behaves like model" $
            (isSpace <$> allWord8)
                == (C.isSpace . word8ToChar <$> allWord8)

    describe "isLower" $ do
        it "behaves like model" $
            (isLower <$> allWord8)
                == (C.isLower . word8ToChar <$> allWord8)

    describe "isUpper" $ do
        it "behaves like model" $
            (isUpper <$> allWord8)
                == (C.isUpper . word8ToChar <$> allWord8)

    describe "isAlpha" $ do
        it "behaves like model" $
            (isAlpha <$> allWord8)
                == (C.isAlpha . word8ToChar <$> allWord8)

    describe "isAlphaNum" $ do
        it "behaves like model" $
            (isAlphaNum <$> allWord8)
                == (C.isAlphaNum . word8ToChar <$> allWord8)

    describe "isPrint" $ do
        it "behaves like model" $
            (isPrint <$> allWord8)
                == (C.isPrint . word8ToChar <$> allWord8)

    describe "isDigit" $ do
        it "behaves like model" $
            (isDigit <$> allWord8)
                == (C.isDigit . word8ToChar <$> allWord8)

    describe "isOctDigit" $ do
        it "behaves like model" $
            (isOctDigit <$> allWord8)
                == (C.isOctDigit . word8ToChar <$> allWord8)

    describe "isHexDigit" $ do
        it "behaves like model" $
            (isHexDigit <$> allWord8)
                == (C.isHexDigit . word8ToChar <$> allWord8)

    describe "isLetter" $ do
        it "behaves like model" $
            (isLetter <$> allWord8)
                == (C.isLetter . word8ToChar <$> allWord8)

    describe "isMark" $ do
        it "behaves like model" $
            (isMark <$> allWord8)
                == (C.isMark . word8ToChar <$> allWord8)

    describe "isNumber" $ do
        it "behaves like model" $
            (isNumber <$> allWord8)
                == (C.isNumber . word8ToChar <$> allWord8)

    describe "isPunctuation" $ do
        it "behaves like model" $
            (isPunctuation <$> allWord8)
                == (C.isPunctuation . word8ToChar <$> allWord8)

    describe "isSymbol" $ do
        it "behaves like model" $
            (isSymbol <$> allWord8)
                == (C.isSymbol . word8ToChar <$> allWord8)

    describe "isSeparator" $ do
        it "behaves like model" $
            (isSeparator <$> allWord8)
                == (C.isSeparator . word8ToChar <$> allWord8)

    describe "isAscii" $ do
        it "behaves like model" $
            (isAscii <$> allWord8)
                == (C.isAscii . word8ToChar <$> allWord8)

    describe "isLatin1" $ do
        it "behaves like model" $
            (isLatin1 <$> allWord8)
                == (C.isLatin1 . word8ToChar <$> allWord8)

    describe "isAsciiUpper" $ do
        it "behaves like model" $
            (isAsciiUpper <$> allWord8)
                == (C.isAsciiUpper . word8ToChar <$> allWord8)

    describe "isAsciiLower" $ do
        it "behaves like model" $
            (isAsciiLower <$> allWord8)
                == (C.isAsciiLower . word8ToChar <$> allWord8)

    describe "toUpper" $ do
        prop "behaves like model" $
            withMaxSuccess 10000 prop_toUpper

    describe "toLower" $ do
        it "behaves like model" $
            (word8ToChar . toLower <$> allWord8)
                == (C.toLower . word8ToChar <$> allWord8)

    describe "toTitle" $ do
        prop "behaves like model" $
            withMaxSuccess 10000 prop_toTitle

prop_toUpper :: Word8 -> Bool
prop_toUpper w
    | w == _mu = True
    | w == _ydieresis = True
    | otherwise = word8ToChar (toUpper w) == C.toUpper (word8ToChar w)

prop_toTitle :: Word8 -> Bool
prop_toTitle w
    | w == _mu = True
    | w == _ydieresis = True
    | otherwise = word8ToChar (toTitle w) == C.toTitle (word8ToChar w)

----------------------------------------------------------------

word8ToChar :: Word8 -> Char
word8ToChar = C.chr . fromIntegral
