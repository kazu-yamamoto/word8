{-# LANGUAGE OverloadedStrings #-}

import Criterion.Main
import qualified Data.ByteString as S
import qualified Data.ByteString.Char8 as S8
import qualified Data.Char
import qualified Data.Char8

main :: IO ()
main = do
    input <- S.readFile "Bench.hs"
    defaultMain [ 
        bench "Data.Char"  $ whnf (S8.map Data.Char.toLower) input        
      , bench "Data.Char8" $ whnf (S8.map Data.Char8.toLower) input
      ]
