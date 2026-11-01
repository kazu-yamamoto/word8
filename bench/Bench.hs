module Main where

import Criterion.Main
import qualified Data.ByteString as S
import qualified Data.ByteString.Char8 as S8
import qualified Data.Char
import qualified Data.Char8
import qualified Data.Word8

import Bench.Extra

main :: IO ()
main = do
    let input = S.concat $ replicate 64 $ S.pack [0 .. 255]
    defaultMain
        [ bench "Data.Char.toLower" $ whnf (S8.map Data.Char.toLower) input
        , bench "Data.Char8.toLower" $ whnf (S8.map Data.Char8.toLower) input
        , bench "Data.Word8.toLower" $ whnf (S.map Data.Word8.toLower) input
        , bench "oldIsAscii" $ whnf (S.all oldIsAscii) input
        , bench "newIsAscii" $ whnf (S.all Data.Word8.isAscii) input
        , bench "baseline" $ whnf (count' (const False)) input
        , bench "oldIsPunctuation" $ whnf (count' oldIsPunctuation) input
        , bench "newIsPunctuation" $ whnf (count' Data.Word8.isPunctuation) input
        , bench "oldIsSymbol" $ whnf (count' oldIsSymbol) input
        , bench "newIsSymbol" $ whnf (count' Data.Word8.isSymbol) input
        ]
  where
    -- count bytes satisfying a predicate
    count' p = S.foldl' (\n w -> if p w then n + 1 else n) (0 :: Int)
