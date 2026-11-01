{-# LANGUAGE MagicHash #-}

-- | Char8 library to be used with "Data.ByteString.Char8".
-- All functions assume that 'Char' is in the Latin-1 range (ISO-8859-1).
-- All utility functions are supposed to work like
-- those of "Data.Char". Exceptions are described in
-- the function documentations.
module Data.Char8 (
    -- * Character classification
    isControl,
    isSpace,
    isLower,
    isUpper,
    isAlpha,
    isAlphaNum,
    isPrint,
    isDigit,
    isOctDigit,
    isHexDigit,
    isLetter,
    isMark,
    isNumber,
    isPunctuation,
    isSymbol,
    isSeparator,

    -- * Subranges
    isAscii,
    isLatin1,
    isAsciiUpper,
    isAsciiLower,

    -- * Case conversion
    toUpper,
    toLower,
    toTitle,
) where

import GHC.Base

import qualified Data.Word8 as W

----------------------------------------------------------------

-- | Is it a control character
isControl :: Char -> Bool
isControl c =
    c <= '\x1f'
        || _del <= c && c <= '\x9f'

-- | Is it white space
isSpace :: Char -> Bool
isSpace c =
    c == _space
        || c == _tab
        || c == _lf
        || c == _cr
        || c == _np
        || c == _vt
        || c == _nbsp

-- | Is it a lower case character
--
-- This function returns 'False' for 170 (ª) and 186 (º).
isLower :: Char -> Bool
isLower c =
    isLower' c
        || c == _mu

isLowerCommon :: Char -> Bool
isLowerCommon c =
    isLower' c
        || c == _mu
        || c == _ordfeminine
        || c == _ordmasculine

isLower' :: Char -> Bool
isLower' c =
    isAsciiLower c
        || _germandbls <= c && c <= _ydieresis && c /= '\xf7'

-- | Is it an upper case character
isUpper :: Char -> Bool
isUpper c =
    isAsciiUpper c
        || _Agrave <= c && c <= _Thorn && c /= '\xd7'

-- | Is it an alphabet letter
--
-- Synonym for 'isLetter'
isAlpha :: Char -> Bool
isAlpha c = isLowerCommon c || isUpper c

-- | Is it an alphabet letter or a number
isAlphaNum :: Char -> Bool
isAlphaNum c = isAlpha c || isNumber c

-- | Is the character printable
isPrint :: Char -> Bool
isPrint c
    | c == _softhyphen = False
isPrint c =
    _space <= c && c <= _tilde
        || _nbsp <= c && c <= _ydieresis

-- | Is the character a digit (@0-9@)
isDigit :: Char -> Bool
isDigit c = '0' <= c && c <= '9'

-- | Is the character an octal digit (@0-7@)
isOctDigit :: Char -> Bool
isOctDigit c = '0' <= c && c <= '7'

-- | Is the character a hexadecimal digit (@0-9a-fA-F@)
isHexDigit :: Char -> Bool
isHexDigit c =
    isDigit c
        || 'A' <= c && c <= 'F'
        || 'a' <= c && c <= 'f'

-- | Is it an alphabet letter
--
-- Synonym for 'isAlpha'
isLetter :: Char -> Bool
isLetter c = isLowerCommon c || isUpper c

-- | A Latin-1 character is never a mark
--
-- Synonymous with @const False@
isMark :: Char -> Bool
isMark _ = False

-- | Is the character a number
isNumber :: Char -> Bool
isNumber c =
    isDigit c
        || c == _s1
        || c == _s2
        || c == _s3
        || c == _1'4
        || c == _1'2
        || c == _3'4

-- | Is it a punctuation character
--
-- This function returns 'True' for 167 (§) and 182 (¶).
isPunctuation :: Char -> Bool
isPunctuation c = isLatin1 c && W.isPunctuation (fromIntegral (ord c))

-- | Is it a symbol character
--
-- This function returns 'False' for 167 (§) and 182 (¶).
isSymbol :: Char -> Bool
isSymbol c = isLatin1 c && W.isSymbol (fromIntegral (ord c))

-- | Is it a (non-breaking) space
isSeparator :: Char -> Bool
isSeparator c =
    c == _space
        || c == _nbsp

----------------------------------------------------------------

-- | Is the character in the ASCII range
isAscii :: Char -> Bool
isAscii c = c <= '\x7f'

-- | Is the character in the Latin-1 range
isLatin1 :: Char -> Bool
isLatin1 c = c <= '\xff'

-- | Is the character an upper case ASCII letter (@A-Z@)
isAsciiUpper :: Char -> Bool
isAsciiUpper c = 'A' <= c && c <= 'Z'

-- | Is the character a lower case ASCII letter (@a-z@)
isAsciiLower :: Char -> Bool
isAsciiLower c = 'a' <= c && c <= 'z'

----------------------------------------------------------------

-- | Changes the character to the upper case variant, if possible.
--
-- Micro sign/mu (µ) and small letter Y with diaeresis (ÿ) remain the same.
toUpper :: Char -> Char
toUpper c@(C# c#)
    | c == _germandbls = c
    | c == _ydieresis = c
    | isLower' c = C# (chr# (ord# c# -# 32#))
    | otherwise = c

-- | Changes the character to the lower case variant, if possible.
toLower :: Char -> Char
toLower c@(C# c#)
    | isUpper c = C# (chr# (ord# c# +# 32#))
    | otherwise = c

-- | Synonym for 'toUpper'
toTitle :: Char -> Char
toTitle = toUpper

----------------------------------------------------------------

_nul, _tab, _lf, _vt, _np, _cr :: Char
_nul = '\x00'
_tab = '\x09'
_lf = '\x0a'
_vt = '\x0b'
_np = '\x0c'
_cr = '\x0d'

_space, _tilde, _del, _nbsp :: Char
_space = '\x20'
_tilde = '\x7e'
_del = '\x7f'
_nbsp = '\xa0'

_ordfeminine, _softhyphen, _mu, _ordmasculine :: Char
_ordfeminine = '\xaa'
_softhyphen = '\xad'
_mu = '\xb5'
_ordmasculine = '\xba'

_s2, _s3, _s1, _1'4, _1'2, _3'4 :: Char
_s2 = '\xb2'
_s3 = '\xb3'
_s1 = '\xb9'
_1'4 = '\xbc'
_1'2 = '\xbd'
_3'4 = '\xbe'

_Agrave, _Thorn :: Char
_Agrave = '\xc0'
_Thorn = '\xde'

_germandbls, _ydieresis :: Char
_germandbls = '\xdf'
_ydieresis = '\xff'
