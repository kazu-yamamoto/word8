{-# LANGUAGE CPP #-}

-- | Word8 library to be used with "Data.ByteString".
-- All functions assume that the 'Word8' is encoded in Latin-1 (ISO-8859-1).
-- All utility functions are supposed to work like those of "Data.Char".
-- Exceptions are described in the function documentations.
--
-- Base library 4.7 (GHC 7.8) or earlier is based on Unicode 6.
--
-- Base library 4.8 (GHC 7.10) or later is based on Unicode 7.
--
-- 'isLower', 'isSymbol' and 'isPunctuation' behave differently.

module Data.Word8 (
  -- * Re-exporting
    Word8

  -- * Functions on bytes

  -- ** Character classification
  , isControl, isSpace, isLower, isUpper
  , isAlpha, isAlphaNum, isPrint, isDigit, isOctDigit, isHexDigit
  , isLetter, isMark, isNumber, isPunctuation, isSymbol, isSeparator

  -- ** Subranges
  , isAscii, isLatin1, isAsciiUpper, isAsciiLower

  -- ** Case conversion
  , toUpper, toLower, toTitle

  -- * Byte constants

  -- ** ASCII characters
  , _nul, _tab, _lf, _vt, _np, _cr
  , _space, _exclam, _quotedbl, _numbersign, _dollar, _percent, _ampersand, _quotesingle, _parenleft, _parenright, _asterisk, _plus, _comma, _hyphen, _period, _slash
  , _0, _1, _2, _3, _4, _5, _6, _7, _8, _9
  , _colon, _semicolon, _less, _equal, _greater, _question, _at
  , _A, _B, _C, _D, _E, _F, _G, _H, _I, _J, _K, _L, _M, _N, _O, _P, _Q, _R, _S, _T, _U, _V, _W, _X, _Y, _Z
  , _bracketleft, _backslash, _bracketright, _circum, _underscore, _grave
  , _a, _b, _c, _d, _e, _f, _g, _h, _i, _j, _k, _l, _m, _n, _o, _p, _q, _r, _s, _t, _u, _v, _w, _x, _y, _z
  , _braceleft, _bar, _braceright, _tilde, _del

  -- ** Some Latin-1 characters
  , _nbsp
  , _ordfeminine, _softhyphen, _mu, _ordmasculine
  , _s2, _s3, _s1, _1'4, _1'2, _3'4
  , _Agrave, _Odieresis, _Oslash, _Thorn
  , _germandbls, _agrave, _odieresis, _oslash, _thorn, _ydieresis
  ) where

import Data.Bits ((.&.))
import Data.Word (Word8)

#ifndef MIN_VERSION_base
#define MIN_VERSION_base(x,y,z) 1
#endif

----------------------------------------------------------------

-- | Is the byte a control character
isControl :: Word8 -> Bool
isControl w = w <= 0x1f
           || _del <= w && w <= 0x9f

-- | Is the byte white space
isSpace :: Word8 -> Bool
isSpace w = w == _space
         || w == _tab
         || w == _lf
         || w == _cr
         || w == _np
         || w == _vt
         || w == _nbsp

-- | Is the byte a lower case character
--
-- This function returns 'True' for 170 (@'ª'@) and 186 (@'º'@) in Unicode 6.
-- But it returns 'False' in Unicode 7.
isLower :: Word8 -> Bool
isLower w = isLower' w
         || w == _mu
#if !MIN_VERSION_base(4,8,0)
         || w == _ordfeminine
         || w == _ordmasculine
#endif

isLowerCommon :: Word8 -> Bool
isLowerCommon w = isLower' w
         || w == _mu
         || w == _ordfeminine
         || w == _ordmasculine

isLower' :: Word8 -> Bool
isLower' w = isAsciiLower w
          || _germandbls <= w && w <= _ydieresis && w /= 0xf7

-- | Is the byte an upper case character
isUpper :: Word8 -> Bool
isUpper w = isAsciiUpper w
         || _Agrave <= w && w <= _Thorn && w /= 0xd7

-- | Is the byte an alphabet letter
--
-- Synonym for 'isLetter'
isAlpha :: Word8 -> Bool
isAlpha w = isLowerCommon w || isUpper w

-- | Is the byte an alphabet letter or a number
isAlphaNum :: Word8 -> Bool
isAlphaNum w = isAlpha w || isNumber w

-- | Is the byte printable
isPrint :: Word8 -> Bool
isPrint w
  | w == _softhyphen = False
isPrint w = _space <= w && w <= _tilde
         || _nbsp  <= w && w <= _ydieresis

-- | Is the byte a digit (@0-9@)
isDigit :: Word8 -> Bool
isDigit w = _0 <= w && w <= _9

-- | Is the byte an octet digit (@0-7@)
isOctDigit :: Word8 -> Bool
isOctDigit w = _0 <= w && w <= _7

-- | Is the byte a hexidecimal digit (@0-9a-zA-Z@)
isHexDigit :: Word8 -> Bool
isHexDigit w = isDigit w
            || _A <= w && w <= _F
            || _a <= w && w <= _f

-- | Is the byte an alphabet letter
--
-- Synonym for 'isAlpha'
isLetter :: Word8 -> Bool
isLetter w = isLowerCommon w || isUpper w

-- | An byte is never a mark
--
-- Synonymous with @const False@
isMark :: Word8 -> Bool
isMark _ = False

-- | Is the byte a number
isNumber :: Word8 -> Bool
isNumber w = isDigit w
          || w == _s1
          || w == _s2
          || w == _s3
          || w == _1'4
          || w == _1'2
          || w == _3'4

-- | Is the byte a punctuation character
--
-- This function returns 'False' for 167 (@'§'@) and 182 (@'¶'@) in Unicode 6.
-- But it returns 'True' in Unicode 7.
isPunctuation :: Word8 -> Bool
isPunctuation w =
    w `elem`
        [0x21,0x22,0x23,0x25,0x26,0x27,0x28,0x29,0x2a,0x2c,0x2d,0x2e,0x2f
        ,0x3a,0x3b,0x3f,0x40,0x5b,0x5c,0x5d,0x5f,0x7b,0x7d
        ,0xa1
#if MIN_VERSION_base(4,8,0)
        ,0xa7,0xb6
#endif
        ,0xab,0xb7,0xbb,0xbf]

-- | Is the byte a symbol character
--
-- This function returns 'True' for 167 (@'§'@) and 182 (@'¶'@) in Unicode 6.
-- But it returns 'False' in Unicode 7.
isSymbol :: Word8 -> Bool
isSymbol w =
    w `elem`
        [0x24,0x2b,0x3c,0x3d,0x3e,0x5e,0x60
        ,0x7c,0x7e
        ,0xa2,0xa3,0xa4,0xa5,0xa6
#if !MIN_VERSION_base(4,8,0)
        ,0xa7,0xb6
#endif
        ,0xa8,0xa9,0xac,0xae,0xaf,0xb0,0xb1,0xb4,0xb8,0xd7,0xf7]

-- | Is the byte a (non-breaking) space
isSeparator :: Word8 -> Bool
isSeparator w = w == _space
             || w == _nbsp

----------------------------------------------------------------

-- | Is the byte in the ASCII range
isAscii :: Word8 -> Bool
isAscii w = w .&. 0x80 == 0

-- | Every byte is in the Latin-1 range
--
-- Synonymous with @const True@
isLatin1 :: Word8 -> Bool
isLatin1 _ = True

-- | Is the byte an upper case ASCII letter (@A-Z@)
isAsciiUpper :: Word8 -> Bool
isAsciiUpper w = _A <= w && w <= _Z

-- | Is the byte a lower case ASCII letter (@a-z@)
isAsciiLower :: Word8 -> Bool
isAsciiLower w = _a <= w && w <= _z

----------------------------------------------------------------

-- | Changes the byte to the upper case variant, if possible.
--
-- Micro sign/mu ('_mu') and small letter Y with diaeresis ('_ydieresis') remain the same.
toUpper :: Word8 -> Word8
toUpper w
  | w == _germandbls = w
  | isLower' w       = w - _space
  | otherwise        = w

-- | Changes the byte to the lower case variant, if possible.
toLower :: Word8 -> Word8
toLower w
  | isUpper w = w + _space
  | otherwise = w

-- | Synonym for 'toUpper'
toTitle :: Word8 -> Word8
toTitle = toUpper

----------------------------------------------------------------

-- | Null character
--
-- > BIN:  0b00000000
-- > OCT:  0o000
-- > DEC:  0
-- > HEX:  0x00
-- > HTML: &#00;
-- > Char: '\NUL'
_nul :: Word8
_nul = 0x00

--- | Start of Heading
--
-- > BIN:  0b00000001
-- > OCT:  0o001
-- > DEC:  1
-- > HEX:  0x01
-- > HTML: &#01;
-- > Char: '\SOH'
-- _soh :: Word8
-- _soh = 0x01

--- | Start of Text
--
-- > BIN:  0b00000010
-- > OCT:  0o002
-- > DEC:  2
-- > HEX:  0x02
-- > HTML: &#02;
-- > Char: '\STX'
-- _stx :: Word8
-- _stx = 0x01

--- | End of Text
--
-- > BIN:  0b00000011
-- > OCT:  0o003
-- > DEC:  3
-- > HEX:  0x03
-- > HTML: &#03;
-- > Char: '\ETX'
-- _etx :: Word8
-- _etx = 0x03

--- | End of Transmission
--
-- > BIN:  0b00000100
-- > OCT:  0o004
-- > DEC:  4
-- > HEX:  0x04
-- > HTML: &#04;
-- > Char: '\EOT'
-- _eot :: Word8
-- _eot = 0x04

--- | Enquiry
--
-- > BIN:  0b00000101
-- > OCT:  0o005
-- > DEC:  5
-- > HEX:  0x05
-- > HTML: &#05;
-- > Char: '\ENQ'
-- _enq :: Word8
-- _enq = 0x05

--- | Acknowledge
--
-- > BIN:  0b00000110
-- > OCT:  0o006
-- > DEC:  6
-- > HEX:  0x06
-- > HTML: &#06;
-- > Char: '\ACK'
-- _ack :: Word8
-- _ack = 0x06

--- | Bell, Alert
--
-- > BIN:  0b00000111
-- > OCT:  0o007
-- > DEC:  7
-- > HEX:  0x07
-- > HTML: &#07;
-- > Char: '\BEL'
-- _bel :: Word8
-- _bel = 0x07

--- | Backspace
--
-- > BIN:  0b00001000
-- > OCT:  0o010
-- > DEC:  8
-- > HEX:  0x08
-- > HTML: &#08;
-- > Char: '\BS'
-- _bs :: Word8
-- _bs = 0x08

--- | Shift Out
--
-- > BIN:  0b00001110
-- > OCT:  0o016
-- > DEC:  14
-- > HEX:  0x0E
-- > HTML: &#14;
-- > Char: '\SO'
-- _so :: Word8
-- _so = 0x0E

--- | Shift In
--
-- > BIN:  0b00001111
-- > OCT:  0o017
-- > DEC:  15
-- > HEX:  0x0F
-- > HTML: &#15;
-- > Char: '\SI'
-- _si :: Word8
-- _si = 0x0F

--- | Data Link Escape
--
-- > BIN:  0b00010000
-- > OCT:  0o020
-- > DEC:  16
-- > HEX:  0x10
-- > HTML: &#16;
-- > Char: '\DLE'
-- _dle :: Word8
-- _dle = 0x10

--- | Device Control One (XON)
--
-- > BIN:  0b00010001
-- > OCT:  0o021
-- > DEC:  17
-- > HEX:  0x11
-- > HTML: &#17;
-- > Char: '\DC1'
-- _dc1 :: Word8
-- _dc1 = 0x11

--- | Device Control Two
--
-- > BIN:  0b00010010
-- > OCT:  0o022
-- > DEC:  18
-- > HEX:  0x12
-- > HTML: &#18;
-- > Char: '\DC2'
-- _dc2 :: Word8
-- _dc2 = 0x12

--- | Device Control Three (XOFF)
--
-- > BIN:  0b00010011
-- > OCT:  0o023
-- > DEC:  19
-- > HEX:  0x13
-- > HTML: &#19;
-- > Char: '\DC3'
-- _dc3 :: Word8
-- _dc3 = 0x13

--- | Device Control Four
--
-- > BIN:  0b00010100
-- > OCT:  0o024
-- > DEC:  20
-- > HEX:  0x14
-- > HTML: &#20;
-- > Char: '\DC4'
-- _dc4 :: Word8
-- _dc4 = 0x14

--- | Negative Acknowledge
--
-- > BIN:  0b00010101
-- > OCT:  0o025
-- > DEC:  21
-- > HEX:  0x15
-- > HTML: &#21;
-- > Char: '\NAK'
-- _nak :: Word8
-- _nak = 0x15

--- | Synchronous Idle
--
-- > BIN:  0b00010110
-- > OCT:  0o026
-- > DEC:  22
-- > HEX:  0x16
-- > HTML: &#22;
-- > Char: '\SYN'
-- _syn :: Word8
-- _syn = 0x16

--- | End of Transmission Block
--
-- > BIN:  0b00010111
-- > OCT:  0o027
-- > DEC:  23
-- > HEX:  0x17
-- > HTML: &#23;
-- > Char: '\ETB'
-- _etb :: Word8
-- _etb = 0x17

--- | Cancel
--
-- > BIN:  0b00011000
-- > OCT:  0o030
-- > DEC:  24
-- > HEX:  0x18
-- > HTML: &#24;
-- > Char: '\CAN'
-- _can :: Word8
-- _can = 0x18

--- | End of medium
--
-- > BIN:  0b00011001
-- > OCT:  0o031
-- > DEC:  25
-- > HEX:  0x19
-- > HTML: &#25;
-- > Char: '\EM'
-- _em :: Word8
-- _em = 0x19

--- | Substitute
--
-- > BIN:  0b00011010
-- > OCT:  0o032
-- > DEC:  26
-- > HEX:  0x1A
-- > HTML: &#26;
-- > Char: '\SUB'
-- _sub :: Word8
-- _sub = 0x1A

--- | Escape
--
-- > BIN:  0b00011011
-- > OCT:  0o033
-- > DEC:  27
-- > HEX:  0x1B
-- > HTML: &#27;
-- > Char: '\ESC'
-- _esc :: Word8
-- _esc = 0x1B

--- | File Separator
--
-- > BIN:  0b00011100
-- > OCT:  0o034
-- > DEC:  28
-- > HEX:  0x1C
-- > HTML: &#28;
-- > Char: '\FS'
-- _fs :: Word8
-- _fs = 0x1C

--- | Group Separator
--
-- > BIN:  0b00011101
-- > OCT:  0o035
-- > DEC:  29
-- > HEX:  0x1D
-- > HTML: &#29;
-- > Char: '\GS'
-- _gs :: Word8
-- _gs = 0x1D

--- | Record Separator
--
-- > BIN:  0b00011110
-- > OCT:  0o036
-- > DEC:  30
-- > HEX:  0x1E
-- > HTML: &#30;
-- > Char: '\RS'
-- _rs :: Word8
-- _rs = 0x1E

--- | Unit Separator
--
-- > BIN:  0b00011111
-- > OCT:  0o037
-- > DEC:  31
-- > HEX:  0x1F
-- > HTML: &#31;
-- > Char: '\US'
-- _us :: Word8
-- _us = 0x1F


-- | Horizontal Tab
--
-- > BIN:  0b00001001
-- > OCT:  0o011
-- > DEC:  9
-- > HEX:  0x09
-- > HTML: &#09;
-- > Char: '\HT' or '\t'
_tab :: Word8
_tab = 0x09

-- | Line Feed
--
-- > BIN:  0b00001010
-- > OCT:  0o012
-- > DEC:  10
-- > HEX:  0x0A
-- > HTML: &#10;
-- > Char: '\LF' or '\n'
_lf :: Word8
_lf = 0x0a

-- | Vertical Tabulation
--
-- > BIN:  0b00001011
-- > OCT:  0o013
-- > DEC:  11
-- > HEX:  0x0B
-- > HTML: &#11;
-- > Char: '\VT'
_vt :: Word8
_vt = 0x0b

-- | Form Feed (New Page)
--
-- > BIN:  0b00001100
-- > OCT:  0o014
-- > DEC:  12
-- > HEX:  0x0C
-- > HTML: &#12;
-- > Char: '\FF'
_np :: Word8
_np = 0x0c

-- | Carriage Return
--
-- > BIN:  0b00001101
-- > OCT:  0o015
-- > DEC:  13
-- > HEX:  0x0D
-- > HTML: &#13;
-- > Char: '\CR' or '\r'
_cr :: Word8
_cr = 0x0d

-- | Space
--
-- > BIN:  0b00100000
-- > OCT:  0o040
-- > DEC:  32
-- > HEX:  0x20
-- > HTML: &#32;
-- > Char: '\SP' or ' '
_space :: Word8
_space = 0x20

-- | Exclamation mark
--
-- > BIN:  0b00100001
-- > OCT:  0o041
-- > DEC:  33
-- > HEX:  0x21
-- > HTML: &#33; or &excl;
-- > Char: '!'
_exclam :: Word8
_exclam = 0x21

-- | Double quotes (or speech marks)
--
-- > BIN:  0b00100010
-- > OCT:  0o042
-- > DEC:  34
-- > HEX:  0x22
-- > HTML: &#34; or &quot;
-- > Char: '"'
_quotedbl :: Word8
_quotedbl = 0x22

-- | Number sign (or hash sign, or pound sign)
--
-- > BIN:  0b00100011
-- > OCT:  0o043
-- > DEC:  35
-- > HEX:  0x23
-- > HTML: &#35; or &num;
-- > Char: '#'
_numbersign :: Word8
_numbersign = 0x23

-- | Dollar
--
-- > BIN:  0b00100100
-- > OCT:  0o044
-- > DEC:  36
-- > HEX:  0x24
-- > HTML: &#36; or &dollar;
-- > Char: '$'
_dollar :: Word8
_dollar = 0x24

-- | Per cent sign
--
-- > BIN:  0b00100101
-- > OCT:  0o045
-- > DEC:  37
-- > HEX:  0x25
-- > HTML: &#37; or &percnt;
-- > Char: '%'
_percent :: Word8
_percent = 0x25

-- | Ampersand
--
-- > BIN:  0b00100110
-- > OCT:  0o046
-- > DEC:  38
-- > HEX:  0x26
-- > HTML: &#38; or &amp;
-- > Char: '&'
_ampersand :: Word8
_ampersand = 0x26

-- | Single quote
--
-- > BIN:  0b00100111
-- > OCT:  0o047
-- > DEC:  39
-- > HEX:  0x27
-- > HTML: &#39; or &apos;
-- > Char: '\''
_quotesingle :: Word8
_quotesingle = 0x27

-- | Open parenthesis (or open bracket)
--
-- > BIN:  0b00101000
-- > OCT:  0o050
-- > DEC:  40
-- > HEX:  0x28
-- > HTML: &#40; or &lparen;
-- > Char: '('
_parenleft :: Word8
_parenleft = 0x28

-- | Close parenthesis (or close bracket)
--
-- > BIN:  0b00101001
-- > OCT:  0o051
-- > DEC:  41
-- > HEX:  0x29
-- > HTML: &#41; or &rparen;
-- > Char: ')'
_parenright :: Word8
_parenright = 0x29

-- | Asterisk
--
-- > BIN:  0b00101010
-- > OCT:  0o052
-- > DEC:  42
-- > HEX:  0x2A
-- > HTML: &#42; or &ast;
-- > Char: '*'
_asterisk :: Word8
_asterisk = 0x2a

-- | Plus
--
-- > BIN:  0b00101011
-- > OCT:  0o053
-- > DEC:  43
-- > HEX:  0x2B
-- > HTML: &#43; or &plus;
-- > Char: '+'
_plus :: Word8
_plus = 0x2b

-- | Comma
--
-- > BIN:  0b00101100
-- > OCT:  0o054
-- > DEC:  44
-- > HEX:  0x2C
-- > HTML: &#44; or &comma;
-- > Char: ','
_comma :: Word8
_comma = 0x2c

-- | Hyphen (or minus, or dash)
--
-- > BIN:  0b00101101
-- > OCT:  0o055
-- > DEC:  45
-- > HEX:  0x2D
-- > HTML: &#45;
-- > Char: '-'
_hyphen :: Word8
_hyphen = 0x2d

-- | Period (or dot, or full stop)
--
-- > BIN:  0b00101110
-- > OCT:  0o056
-- > DEC:  46
-- > HEX:  0x2E
-- > HTML: &#46; or &period;
-- > Char: '.'
_period :: Word8
_period = 0x2e

-- | Slash (or divide)
--
-- > BIN:  0b00101111
-- > OCT:  0o057
-- > DEC:  47
-- > HEX:  0x2F
-- > HTML: &#47; or &sol;
-- > Char: '/'
_slash :: Word8
_slash = 0x2f


-- | Zero
--
-- > BIN:  0b00110000
-- > OCT:  0o060
-- > DEC:  48
-- > HEX:  0x30
-- > HTML: &#48;
-- > Char: '0'
_0 :: Word8
_0 = 0x30

-- | One
--
-- > BIN:  0b00110001
-- > OCT:  0o061
-- > DEC:  49
-- > HEX:  0x31
-- > HTML: &#49;
-- > Char: '1'
_1 :: Word8
_1 = 0x31

-- | Two
--
-- > BIN:  0b00110010
-- > OCT:  0o062
-- > DEC:  50
-- > HEX:  0x32
-- > HTML: &#50;
-- > Char: '2'
_2 :: Word8
_2 = 0x32

-- | Three
--
-- > BIN:  0b00110011
-- > OCT:  0o063
-- > DEC:  51
-- > HEX:  0x33
-- > HTML: &#51;
-- > Char: '3'
_3 :: Word8
_3 = 0x33

-- | Four
--
-- > BIN:  0b00110100
-- > OCT:  0o064
-- > DEC:  52
-- > HEX:  0x34
-- > HTML: &#52;
-- > Char: '4'
_4 :: Word8
_4 = 0x34

-- | Five
--
-- > BIN:  0b00110101
-- > OCT:  0o065
-- > DEC:  53
-- > HEX:  0x35
-- > HTML: &#53;
-- > Char: '5'
_5 :: Word8
_5 = 0x35

-- | Six
--
-- > BIN:  0b00110110
-- > OCT:  0o066
-- > DEC:  54
-- > HEX:  0x36
-- > HTML: &#54;
-- > Char: '6'
_6 :: Word8
_6 = 0x36

-- | Seven
--
-- > BIN:  0b00110111
-- > OCT:  0o067
-- > DEC:  55
-- > HEX:  0x37
-- > HTML: &#55;
-- > Char: '7'
_7 :: Word8
_7 = 0x37

-- | Eight
--
-- > BIN:  0b00111000
-- > OCT:  0o070
-- > DEC:  56
-- > HEX:  0x38
-- > HTML: &#56;
-- > Char: '8'
_8 :: Word8
_8 = 0x38

-- | Nine
--
-- > BIN:  0b00111001
-- > OCT:  0o071
-- > DEC:  57
-- > HEX:  0x39
-- > HTML: &#57;
-- > Char: '9'
_9 :: Word8
_9 = 0x39

-- | Colon
--
-- > BIN:  0b00111010
-- > OCT:  0o072
-- > DEC:  58
-- > HEX:  0x3A
-- > HTML: &#58; or &colon;
-- > Char: ':'
_colon :: Word8
_colon = 0x3a

-- | Semicolon
--
-- > BIN:  0b00111011
-- > OCT:  0o073
-- > DEC:  59
-- > HEX:  0x3B
-- > HTML: &#59; or &semi;
-- > Char: ';'
_semicolon :: Word8
_semicolon = 0x3b

-- | Less than (or open angled bracket)
--
-- > BIN:  0b00111100
-- > OCT:  0o074
-- > DEC:  60
-- > HEX:  0x3C
-- > HTML: &#60; or &lt;
-- > Char: '<'
_less :: Word8
_less = 0x3c

-- | Equals
--
-- > BIN:  0b00111101
-- > OCT:  0o075
-- > DEC:  61
-- > HEX:  0x3D
-- > HTML: &#61; or &equals;
-- > Char: '='
_equal :: Word8
_equal = 0x3d

-- | Greater than (or close angled bracket)
--
-- > BIN:  0b00111110
-- > OCT:  0o076
-- > DEC:  62
-- > HEX:  0x3E
-- > HTML: &#62; or &gt;
-- > Char: '>'
_greater :: Word8
_greater = 0x3e

-- | Question mark
--
-- > BIN:  0b00111111
-- > OCT:  0o077
-- > DEC:  63
-- > HEX:  0x3F
-- > HTML: &#63; or &quest;
-- > Char: '?'
_question :: Word8
_question = 0x3f

-- | At sign
--
-- > BIN:  0b01000000
-- > OCT:  0o100
-- > DEC:  64
-- > HEX:  0x40
-- > HTML: &#64; or &commat;
-- > Char: '@'
_at :: Word8
_at = 0x40

-- | Uppercase A
--
-- > BIN:  0b01000001
-- > OCT:  0o101
-- > DEC:  65
-- > HEX:  0x41
-- > HTML: &#65;
-- > Char: 'A'
_A :: Word8
_A = 0x41

-- | Uppercase B
--
-- > BIN:  0b01000010
-- > OCT:  0o102
-- > DEC:  66
-- > HEX:  0x42
-- > HTML: &#66;
-- > Char: 'B'
_B :: Word8
_B = 0x42

-- | Uppercase C
--
-- > BIN:  0b01000011
-- > OCT:  0o103
-- > DEC:  67
-- > HEX:  0x43
-- > HTML: &#67;
-- > Char: 'C'
_C :: Word8
_C = 0x43

-- | Uppercase D
--
-- > BIN:  0b01000100
-- > OCT:  0o104
-- > DEC:  68
-- > HEX:  0x44
-- > HTML: &#68;
-- > Char: 'D'
_D :: Word8
_D = 0x44

-- | Uppercase E
--
-- > BIN:  0b01000101
-- > OCT:  0o105
-- > DEC:  69
-- > HEX:  0x45
-- > HTML: &#69;
-- > Char: 'E'
_E :: Word8
_E = 0x45

-- | Uppercase F
--
-- > BIN:  0b01000110
-- > OCT:  0o106
-- > DEC:  70
-- > HEX:  0x46
-- > HTML: &#70;
-- > Char: 'F'
_F :: Word8
_F = 0x46

-- | Uppercase G
--
-- > BIN:  0b01000111
-- > OCT:  0o107
-- > DEC:  71
-- > HEX:  0x47
-- > HTML: &#71;
-- > Char: 'G'
_G :: Word8
_G = 0x47

-- | Uppercase H
--
-- > BIN:  0b01001000
-- > OCT:  0o110
-- > DEC:  72
-- > HEX:  0x48
-- > HTML: &#72;
-- > Char: 'H'
_H :: Word8
_H = 0x48

-- | Uppercase I
--
-- > BIN:  0b01001001
-- > OCT:  0o111
-- > DEC:  73
-- > HEX:  0x49
-- > HTML: &#73;
-- > Char: 'I'
_I :: Word8
_I = 0x49

-- | Uppercase J
--
-- > BIN:  0b01001010
-- > OCT:  0o112
-- > DEC:  74
-- > HEX:  0x4A
-- > HTML: &#74;
-- > Char: 'J'
_J :: Word8
_J = 0x4a

-- | Uppercase K
--
-- > BIN:  0b01001011
-- > OCT:  0o113
-- > DEC:  75
-- > HEX:  0x4B
-- > HTML: &#75;
-- > Char: 'K'
_K :: Word8
_K = 0x4b

-- | Uppercase L
--
-- > BIN:  0b01001100
-- > OCT:  0o114
-- > DEC:  76
-- > HEX:  0x4C
-- > HTML: &#76;
-- > Char: 'L'
_L :: Word8
_L = 0x4c

-- | Uppercase M
--
-- > BIN:  0b01001101
-- > OCT:  0o115
-- > DEC:  77
-- > HEX:  0x4D
-- > HTML: &#77;
-- > Char: 'M'
_M :: Word8
_M = 0x4d

-- | Uppercase N
--
-- > BIN:  0b01001110
-- > OCT:  0o116
-- > DEC:  78
-- > HEX:  0x4E
-- > HTML: &#78;
-- > Char: 'N'
_N :: Word8
_N = 0x4e

-- | Uppercase O
--
-- > BIN:  0b01001111
-- > OCT:  0o117
-- > DEC:  79
-- > HEX:  0x4F
-- > HTML: &#79;
-- > Char: 'O'
_O :: Word8
_O = 0x4f

-- | Uppercase P
--
-- > BIN:  0b01010000
-- > OCT:  0o120
-- > DEC:  80
-- > HEX:  0x50
-- > HTML: &#80;
-- > Char: 'P'
_P :: Word8
_P = 0x50

-- | Uppercase Q
--
-- > BIN:  0b01010001
-- > OCT:  0o121
-- > DEC:  81
-- > HEX:  0x51
-- > HTML: &#81;
-- > Char: 'Q'
_Q :: Word8
_Q = 0x51

-- | Uppercase R
--
-- > BIN:  0b01010010
-- > OCT:  0o122
-- > DEC:  82
-- > HEX:  0x52
-- > HTML: &#82;
-- > Char: 'R'
_R :: Word8
_R = 0x52

-- | Uppercase S
--
-- > BIN:  0b01010011
-- > OCT:  0o123
-- > DEC:  83
-- > HEX:  0x53
-- > HTML: &#83;
-- > Char: 'S'
_S :: Word8
_S = 0x53

-- | Uppercase T
--
-- > BIN:  0b01010100
-- > OCT:  0o124
-- > DEC:  84
-- > HEX:  0x54
-- > HTML: &#84;
-- > Char: 'T'
_T :: Word8
_T = 0x54

-- | Uppercase U
--
-- > BIN:  0b01010101
-- > OCT:  0o125
-- > DEC:  85
-- > HEX:  0x55
-- > HTML: &#85;
-- > Char: 'U'
_U :: Word8
_U = 0x55

-- | Uppercase V
--
-- > BIN:  0b01010110
-- > OCT:  0o126
-- > DEC:  86
-- > HEX:  0x56
-- > HTML: &#86;
-- > Char: 'V'
_V :: Word8
_V = 0x56

-- | Uppercase W
--
-- > BIN:  0b01010111
-- > OCT:  0o127
-- > DEC:  87
-- > HEX:  0x57
-- > HTML: &#87;
-- > Char: 'W'
_W :: Word8
_W = 0x57

-- | Uppercase X
--
-- > BIN:  0b01011000
-- > OCT:  0o130
-- > DEC:  88
-- > HEX:  0x58
-- > HTML: &#88;
-- > Char: 'X'
_X :: Word8
_X = 0x58

-- | Uppercase Y
--
-- > BIN:  0b01011001
-- > OCT:  0o131
-- > DEC:  89
-- > HEX:  0x59
-- > HTML: &#89;
-- > Char: 'Y'
_Y :: Word8
_Y = 0x59

-- | Uppercase Z
--
-- > BIN:  0b01011010
-- > OCT:  0o132
-- > DEC:  90
-- > HEX:  0x5A
-- > HTML: &#90;
-- > Char: 'Z'
_Z :: Word8
_Z = 0x5a

-- | Opening bracket
--
-- > BIN:  0b01011011
-- > OCT:  0o133
-- > DEC:  91
-- > HEX:  0x5B
-- > HTML: &#91; or &lsqb;
-- > Char: '['
_bracketleft :: Word8
_bracketleft = 0x5b

-- | Backslash
--
-- > BIN:  0b01011100
-- > OCT:  0o134
-- > DEC:  92
-- > HEX:  0x5C
-- > HTML: &#92; or &bsol;
-- > Char: '\'
_backslash :: Word8
_backslash = 0x5c

-- | Closing bracket
--
-- > BIN:  0b01011101
-- > OCT:  0o135
-- > DEC:  93
-- > HEX:  0x5D
-- > HTML: &#93; or &rsqb;
-- > Char: ']'
_bracketright :: Word8
_bracketright = 0x5d

-- | Caret - circumflex
--
-- > BIN:  0b01011110
-- > OCT:  0o136
-- > DEC:  94
-- > HEX:  0x5E
-- > HTML: &#94; or &Hat;
-- > Char: '^'
_circum :: Word8
_circum = 0x5e

-- | Underscore
--
-- > BIN:  0b01011111
-- > OCT:  0o137
-- > DEC:  95
-- > HEX:  0x5F
-- > HTML: &#95; or &lowbar;
-- > Char: '_'
_underscore :: Word8
_underscore = 0x5f


-- | Grave accent
--
-- > BIN:  0b01100000
-- > OCT:  0o140
-- > DEC:  96
-- > HEX:  0x60
-- > HTML: &#96; or &grave;
-- > Char: '`'
_grave :: Word8
_grave = 0x60

-- | Lowercase a
--
-- > BIN:  0b01100001
-- > OCT:  0o141
-- > DEC:  97
-- > HEX:  0x61
-- > HTML: &#97;
-- > Char: 'a'
_a :: Word8
_a = 0x61

-- | Lowercase b
--
-- > BIN:  0b01100010
-- > OCT:  0o142
-- > DEC:  98
-- > HEX:  0x62
-- > HTML: &#98;
-- > Char: 'b'
_b :: Word8
_b = 0x62

-- | Lowercase c
--
-- > BIN:  0b01100011
-- > OCT:  0o143
-- > DEC:  99
-- > HEX:  0x63
-- > HTML: &#99;
-- > Char: 'c'
_c :: Word8
_c = 0x63

-- | Lowercase d
--
-- > BIN:  0b01100100
-- > OCT:  0o144
-- > DEC:  100
-- > HEX:  0x64
-- > HTML: &#100;
-- > Char: 'd'
_d :: Word8
_d = 0x64

-- | Lowercase e
--
-- > BIN:  0b01100101
-- > OCT:  0o145
-- > DEC:  101
-- > HEX:  0x65
-- > HTML: &#101;
-- > Char: 'e'
_e :: Word8
_e = 0x65

-- | Lowercase f
--
-- > BIN:  0b01100110
-- > OCT:  0o146
-- > DEC:  102
-- > HEX:  0x66
-- > HTML: &#102;
-- > Char: 'f'
_f :: Word8
_f = 0x66

-- | Lowercase g
--
-- > BIN:  0b01100111
-- > OCT:  0o147
-- > DEC:  103
-- > HEX:  0x67
-- > HTML: &#103;
-- > Char: 'g'
_g :: Word8
_g = 0x67

-- | Lowercase h
--
-- > BIN:  0b01101000
-- > OCT:  0o150
-- > DEC:  104
-- > HEX:  0x68
-- > HTML: &#104;
-- > Char: 'h'
_h :: Word8
_h = 0x68

-- | Lowercase i
--
-- > BIN:  0b01101001
-- > OCT:  0o151
-- > DEC:  105
-- > HEX:  0x69
-- > HTML: &#105;
-- > Char: 'i'
_i :: Word8
_i = 0x69

-- | Lowercase j
--
-- > BIN:  0b01101010
-- > OCT:  0o152
-- > DEC:  106
-- > HEX:  0x6A
-- > HTML: &#106;
-- > Char: 'j'
_j :: Word8
_j = 0x6a

-- | Lowercase k
--
-- > BIN:  0b01101011
-- > OCT:  0o153
-- > DEC:  107
-- > HEX:  0x6B
-- > HTML: &#107;
-- > Char: 'k'
_k :: Word8
_k = 0x6b

-- | Lowercase l
--
-- > BIN:  0b01101100
-- > OCT:  0o154
-- > DEC:  108
-- > HEX:  0x6C
-- > HTML: &#108;
-- > Char: 'l'
_l :: Word8
_l = 0x6c

-- | Lowercase m
--
-- > BIN:  0b01101101
-- > OCT:  0o155
-- > DEC:  109
-- > HEX:  0x6D
-- > HTML: &#109;
-- > Char: 'm'
_m :: Word8
_m = 0x6d

-- | Lowercase n
--
-- > BIN:  0b01101110
-- > OCT:  0o156
-- > DEC:  110
-- > HEX:  0x6E
-- > HTML: &#110;
-- > Char: 'n'
_n :: Word8
_n = 0x6e

-- | Lowercase o
--
-- > BIN:  0b01101111
-- > OCT:  0o157
-- > DEC:  111
-- > HEX:  0x6F
-- > HTML: &#111;
-- > Char: 'o'
_o :: Word8
_o = 0x6f

-- | Lowercase p
--
-- > BIN:  0b01110000
-- > OCT:  0o160
-- > DEC:  112
-- > HEX:  0x70
-- > HTML: &#112;
-- > Char: 'p'
_p :: Word8
_p = 0x70

-- | Lowercase q
--
-- > BIN:  0b01110001
-- > OCT:  0o161
-- > DEC:  113
-- > HEX:  0x71
-- > HTML: &#113;
-- > Char: 'q'
_q :: Word8
_q = 0x71

-- | Lowercase r
--
-- > BIN:  0b01110010
-- > OCT:  0o162
-- > DEC:  114
-- > HEX:  0x72
-- > HTML: &#114;
-- > Char: 'r'
_r :: Word8
_r = 0x72

-- | Lowercase s
--
-- > BIN:  0b01110011
-- > OCT:  0o163
-- > DEC:  115
-- > HEX:  0x73
-- > HTML: &#115;
-- > Char: 's'
_s :: Word8
_s = 0x73

-- | Lowercase t
--
-- > BIN:  0b01110100
-- > OCT:  0o164
-- > DEC:  116
-- > HEX:  0x74
-- > HTML: &#116;
-- > Char: 't'
_t :: Word8
_t = 0x74

-- | Lowercase u
--
-- > BIN:  0b01110101
-- > OCT:  0o165
-- > DEC:  117
-- > HEX:  0x75
-- > HTML: &#117;
-- > Char: 'u'
_u :: Word8
_u = 0x75

-- | Lowercase v
--
-- > BIN:  0b01110110
-- > OCT:  0o166
-- > DEC:  118
-- > HEX:  0x76
-- > HTML: &#118;
-- > Char: 'v'
_v :: Word8
_v = 0x76

-- | Lowercase w
--
-- > BIN:  0b01110111
-- > OCT:  0o167
-- > DEC:  119
-- > HEX:  0x77
-- > HTML: &#119;
-- > Char: 'w'
_w :: Word8
_w = 0x77

-- | Lowercase x
--
-- > BIN:  0b01111000
-- > OCT:  0o170
-- > DEC:  120
-- > HEX:  0x78
-- > HTML: &#120;
-- > Char: 'x'
_x :: Word8
_x = 0x78

-- | Lowercase y
--
-- > BIN:  0b01111001
-- > OCT:  0o171
-- > DEC:  121
-- > HEX:  0x79
-- > HTML: &#121;
-- > Char: 'y'
_y :: Word8
_y = 0x79

-- | Lowercase z
--
-- > BIN:  0b01111010
-- > OCT:  0o172
-- > DEC:  122
-- > HEX:  0x7A
-- > HTML: &#122;
-- > Char: 'z'
_z :: Word8
_z = 0x7a

-- | Opening brace
--
-- > BIN:  0b01111011
-- > OCT:  0o173
-- > DEC:  123
-- > HEX:  0x7B
-- > HTML: &#123; or &lcub;
-- > Char: '{'
_braceleft :: Word8
_braceleft = 0x7b

-- | Vertical bar
--
-- > BIN:  0b01111100
-- > OCT:  0o174
-- > DEC:  124
-- > HEX:  0x7C
-- > HTML: &#124; or &verbar;
-- > Char: '|'
_bar :: Word8
_bar = 0x7c

-- | Closing brace
--
-- > BIN:  0b01111101
-- > OCT:  0o175
-- > DEC:  125
-- > HEX:  0x7D
-- > HTML: &#125; or &rcub;
-- > Char: '}'
_braceright :: Word8
_braceright = 0x7d

-- | Equivalency sign - tilde
--
-- > BIN:  0b01111110
-- > OCT:  0o176
-- > DEC:  126
-- > HEX:  0x7E
-- > HTML: &#126; or &tilde;
-- > Char: '~'
_tilde :: Word8
_tilde = 0x7e

-- | Delete
--
-- > BIN:  0b01111111
-- > OCT:  0o177
-- > DEC:  127
-- > HEX:  0x7F
-- > HTML: &#127;
-- > Char: '\DEL'
_del :: Word8
_del = 0x7f

-- | Non-breaking space
--
-- > BIN:  0b10100000
-- > OCT:  0o240
-- > DEC:  160
-- > HEX:  0xA0
-- > HTML: &#160; or &nbsp;
_nbsp :: Word8
_nbsp = 0xa0

-- | Feminine ordinal indicator
--
-- > BIN:  0b10101010
-- > OCT:  0o252
-- > DEC:  170
-- > HEX:  0xAA
-- > HTML: &#170; or &ordf;
-- > Char: 'ª'
_ordfeminine :: Word8
_ordfeminine = 0xaa

-- | Soft hyphen
--
-- > BIN:  0b10101101
-- > OCT:  0o255
-- > DEC:  173
-- > HEX:  0xAD
-- > HTML: &#173; or &shy;
_softhyphen :: Word8
_softhyphen = 0xad

-- | Micro sign (or greek letter mu)
--
-- > BIN:  0b10110101
-- > OCT:  0o265
-- > DEC:  181
-- > HEX:  0xB5
-- > HTML: &#181; or &micro;
-- > Char: 'µ'
_mu :: Word8
_mu = 0xb5

-- | Masculine ordinal indicator
--
-- > BIN:  0b10111010
-- > OCT:  0o272
-- > DEC:  186
-- > HEX:  0xBA
-- > HTML: &#186; or &ordm;
-- > Char: 'º'
_ordmasculine :: Word8
_ordmasculine = 0xba

-- | Superscript two - squared
--
-- > BIN:  0b10110010
-- > OCT:  0o262
-- > DEC:  178
-- > HEX:  0xB2
-- > HTML: &#178; or &sup2;
-- > Char: '²'
_s2 :: Word8
_s2 = 0xb2

-- | Superscript three - cubed
--
-- > BIN:  0b10110011
-- > OCT:  0o263
-- > DEC:  179
-- > HEX:  0xB3
-- > HTML: &#179; or &sup3;
-- > Char: '³'
_s3 :: Word8
_s3 = 0xb3

-- | Superscript one
--
-- > BIN:  0b10111001
-- > OCT:  0o271
-- > DEC:  185
-- > HEX:  0xB9
-- > HTML: &#185; or &sup1;
-- > Char: '¹'
_s1 :: Word8
_s1 = 0xb9

-- | Fraction one quarter
--
-- > BIN:  0b10111100
-- > OCT:  0o274
-- > DEC:  188
-- > HEX:  0xBC
-- > HTML: &#188; or &frac14;
-- > Char: '¼'
_1'4 :: Word8
_1'4 = 0xbc

-- | Fraction one half
--
-- > BIN:  0b10111101
-- > OCT:  0o275
-- > DEC:  189
-- > HEX:  0xBD
-- > HTML: &#189; or &frac12;
-- > Char: '½'
_1'2 :: Word8
_1'2 = 0xbd

-- | Fraction three quarters
--
-- > BIN:  0b10111110
-- > OCT:  0o276
-- > DEC:  190
-- > HEX:  0xBE
-- > HTML: &#190; or &frac34;
-- > Char: '¾'
_3'4  :: Word8
_3'4 = 0xbe

-- | Latin capital letter A with grave
--
-- > BIN:  0b11000000
-- > OCT:  0o300
-- > DEC:  192
-- > HEX:  0xC0
-- > HTML: &#192; or &Agrave;
-- > Char: 'À'
_Agrave :: Word8
_Agrave = 0xc0

-- | Latin capital letter O with diaeresis
--
-- > BIN:  0b11010110
-- > OCT:  0o326
-- > DEC:  214
-- > HEX:  0xD6
-- > HTML: &#214; or &Ouml;
-- > Char: 'Ö'
_Odieresis :: Word8
_Odieresis = 0xd6

-- | Latin capital letter O with slash
--
-- > BIN:  0b11011000
-- > OCT:  0o330
-- > DEC:  216
-- > HEX:  0xD8
-- > HTML: &#216; or &Oslash;
-- > Char: 'Ø'
_Oslash :: Word8
_Oslash = 0xd8

-- | Latin capital letter THORN
--
-- > BIN:  0b11011110
-- > OCT:  0o336
-- > DEC:  222
-- > HEX:  0xDE
-- > HTML: &#222; or &THORN;
-- > Char: 'Þ'
_Thorn :: Word8
_Thorn = 0xde


-- | Latin small letter sharp s - ess-zed
--
-- > BIN:  0b11011111
-- > OCT:  0o337
-- > DEC:  223
-- > HEX:  0xDF
-- > HTML: &#223; or &szlig;
-- > Char: 'ß'
_germandbls :: Word8
_germandbls = 0xdf

-- | Latin small letter a with grave
--
-- > BIN:  0b11100000
-- > OCT:  0o340
-- > DEC:  224
-- > HEX:  0xE0
-- > HTML: &#224; or &agrave;
-- > Char: 'à'
_agrave :: Word8
_agrave = 0xe0

-- | Latin small letter o with diaeresis
--
-- > BIN:  0b11110110
-- > OCT:  0o366
-- > DEC:  246
-- > HEX:  0xF6
-- > HTML: &#246; or &ouml;
-- > Char: 'ö'
_odieresis :: Word8
_odieresis = 0xf6

-- | Latin small letter o with slash
--
-- > BIN:  0b11111000
-- > OCT:  0o370
-- > DEC:  248
-- > HEX:  0xF8
-- > HTML: &#248; or &oslash;
-- > Char: 'ø'
_oslash :: Word8
_oslash = 0xf8

-- | Latin small letter thorn
--
-- > BIN:  0b11111110
-- > OCT:  0o376
-- > DEC:  254
-- > HEX:  0xFE
-- > HTML: &#254; or &thorn;
-- > Char: 'þ'
_thorn :: Word8
_thorn = 0xfe

-- | Latin small letter y with diaeresis
--
-- > BIN:  0b11111111
-- > OCT:  0o377
-- > DEC:  255
-- > HEX:  0xFF
-- > HTML: &#255; or &yuml;
-- > Char: 'ÿ'
_ydieresis :: Word8
_ydieresis = 0xff
