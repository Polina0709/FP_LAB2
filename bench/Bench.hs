{-# LANGUAGE BangPatterns #-}
module Main where

import Criterion.Main
import Sort (bitonicSort, bitonicSortParallel, genList)
import Control.DeepSeq (force)

main :: IO ()
main = do
    let !input100k = genList 100000
        !input200k = genList 200000

    let !i100k = force input100k
        !i200k = force input200k

    defaultMain
      [ bgroup "100k elements"
          [ bench "sequential"        $ nf bitonicSort                  i100k
          , bench "parallel k=2"      $ nf (bitonicSortParallel 2)      i100k
          , bench "parallel k=4"      $ nf (bitonicSortParallel 4)      i100k
          , bench "parallel k=8"      $ nf (bitonicSortParallel 8)      i100k
          ]

      , bgroup "200k elements"
          [ bench "sequential"        $ nf bitonicSort                  i200k
          , bench "parallel k=2"      $ nf (bitonicSortParallel 2)      i200k
          , bench "parallel k=4"      $ nf (bitonicSortParallel 4)      i200k
          , bench "parallel k=8"      $ nf (bitonicSortParallel 8)      i200k
          ]
      ]
