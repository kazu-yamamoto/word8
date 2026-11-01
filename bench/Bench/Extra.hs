module Bench.Extra where

import Data.Word8

oldIsAscii :: Word8 -> Bool
oldIsAscii w = _nul <= w && w <= _del

oldIsPunctuation :: Word8 -> Bool
oldIsPunctuation w =
    w
        `elem` [ 0x21
               , 0x22
               , 0x23
               , 0x25
               , 0x26
               , 0x27
               , 0x28
               , 0x29
               , 0x2a
               , 0x2c
               , 0x2d
               , 0x2e
               , 0x2f
               , 0x3a
               , 0x3b
               , 0x3f
               , 0x40
               , 0x5b
               , 0x5c
               , 0x5d
               , 0x5f
               , 0x7b
               , 0x7d
               , 0xa1
               , 0xa7
               , 0xb6
               , 0xab
               , 0xb7
               , 0xbb
               , 0xbf
               ]

oldIsSymbol :: Word8 -> Bool
oldIsSymbol w =
    w
        `elem` [ 0x24
               , 0x2b
               , 0x3c
               , 0x3d
               , 0x3e
               , 0x5e
               , 0x60
               , 0x7c
               , 0x7e
               , 0xa2
               , 0xa3
               , 0xa4
               , 0xa5
               , 0xa6
               , 0xa8
               , 0xa9
               , 0xac
               , 0xae
               , 0xaf
               , 0xb0
               , 0xb1
               , 0xb4
               , 0xb8
               , 0xd7
               , 0xf7
               ]
