module Bench.Extra where

import Data.Word8

oldIsAscii :: Word8 -> Bool
oldIsAscii w = _nul <= w && w <= _del
