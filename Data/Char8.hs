{-# LANGUAGE CPP #-}
{-# LANGUAGE MagicHash #-}

-- | Char8 library to be used with "Data.ByteString.Char8".
-- All functions assume that only the 8-bit part of 'Char'.
-- All utility functions are supposed to work like
-- those of "Data.Char". Exceptions are described in
-- the function documentations.
--
-- Base library 4.7 (GHC 7.8) or earlier is based on Unicode 6.
--
-- Base library 4.8 (GHC 7.10) or later is based on Unicode 7.
--
-- 'isLower', 'isSymbol' and 'isPunctuation' behave differently.
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
-- This function returns 'True' for 170 (@'ª'@) and 186 (@'º'@) in Unicode 6.
-- But it returns 'False' in Unicode 7.
isLower :: Char -> Bool
isLower c =
    isLower' c
        || c == _mu
#if !MIN_VERSION_base(4,8,0)
        || c == _ordfeminine
        || c == _ordmasculine

#endif

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

-- | Is the character an octet digit (@0-7@)
isOctDigit :: Char -> Bool
isOctDigit c = '0' <= c && c <= '7'

-- | Is the character a hexidecimal digit (@0-9a-zA-Z@)
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
-- This function returns 'False' for 167 (@'§'@) and 182 (@'¶'@) in Unicode 6.
-- But it returns 'True' in Unicode 7.
isPunctuation :: Char -> Bool
isPunctuation c =
    c
        `elem` [ '\x21'
               , '\x22'
               , '\x23'
               , '\x25'
               , '\x26'
               , '\x27'
               , '\x28'
               , '\x29'
               , '\x2a'
               , '\x2c'
               , '\x2d'
               , '\x2e'
               , '\x2f'
               , '\x3a'
               , '\x3b'
               , '\x3f'
               , '\x40'
               , '\x5b'
               , '\x5c'
               , '\x5d'
               , '\x5f'
               , '\x7b'
               , '\x7d'
#if MIN_VERSION_base(4,8,0)
               , '\xa7'
               , '\xb6'
#endif
               , '\xa1'
               , '\xab'
               , '\xb7'
               , '\xbb'
               , '\xbf'
               ]

-- | Is it a symbol character
--
-- This function returns 'True' for 167 (@'§'@) and 182 (@'¶'@) in Unicode 6.
-- But it returns 'False' in Unicode 7.
isSymbol :: Char -> Bool
isSymbol c =
    c
        `elem` [ '\x24'
               , '\x2b'
               , '\x3c'
               , '\x3d'
               , '\x3e'
               , '\x5e'
               , '\x60'
               , '\x7c'
               , '\x7e'
               , '\xa2'
               , '\xa3'
               , '\xa4'
               , '\xa5'
               , '\xa6'
#if !MIN_VERSION_base(4,8,0)
               , '\xa7'
               , '\xb6'
#endif
               , '\xa8'
               , '\xa9'
               , '\xac'
               , '\xae'
               , '\xaf'
               , '\xb0'
               , '\xb1'
               , '\xb4'
               , '\xb8'
               , '\xd7'
               , '\xf7'
               ]

-- | Is it a (non-breaking) space
isSeparator :: Char -> Bool
isSeparator c =
    c == _space
        || c == _nbsp

----------------------------------------------------------------

-- | Is the character in the ASCII range
isAscii :: Char -> Bool
#if __GLASGOW_HASKELL__ >= 707
isAscii (C# c#) = isTrue# (ord# c# <=# 0x7f#)
#else
isAscii (C# c#) = ord# c# <=# 0x7f#
#endif

-- | Is the character in the Latin-1 range
isLatin1 :: Char -> Bool
#if __GLASGOW_HASKELL__ >= 707
isLatin1 (C# c#) = isTrue# (ord# c# <=# 0xff#)
#else
isLatin1 (C# c#) = ord# c# <=# 0xff#
#endif

-- | Is the character an upper case ASCII letter (@A-Z@)
isAsciiUpper :: Char -> Bool
isAsciiUpper c = 'A' <= c && c <= 'Z'

-- | Is the character a lower case ASCII letter (@a-z@)
isAsciiLower :: Char -> Bool
isAsciiLower c = 'a' <= c && c <= 'z'

----------------------------------------------------------------

-- | Changes the character to the upper case variant, if possible.
--
-- Micro sign/mu ('_mu') and small letter Y with diaeresis ('_ydieresis') remain the same.
toUpper :: Char -> Char
toUpper c@(C# c#)
    | c == _germandbls = c
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
