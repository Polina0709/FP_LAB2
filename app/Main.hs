{-# LANGUAGE BangPatterns #-}
module Main (main) where

import Sort (bitonicSort)
import GHC.Conc (setNumCapabilities)
import System.CPUTime (getCPUTime)
import Text.Printf (printf)
import System.Random (mkStdGen, randomRs)
import Control.Exception (evaluate)
import Control.DeepSeq (NFData, force)

----------------------------------------------------
-- Generate deterministic random list
----------------------------------------------------
genList :: Int -> [Int]
genList n = take n $ randomRs (1, 1000000) (mkStdGen 42)

----------------------------------------------------
-- Force full list evaluation
----------------------------------------------------
forceList :: NFData a => [a] -> IO ()
forceList xs = evaluate (force xs) >> return ()

----------------------------------------------------
-- Execution time measurement
----------------------------------------------------
time :: IO a -> IO a
time action = do
    start <- getCPUTime
    result <- action
    end <- getCPUTime
    let diff = fromIntegral (end - start) / (10 ** 12 :: Double)
    printf "\nExecution time: %.5f sec\n" diff
    return result

----------------------------------------------------
-- MAIN
----------------------------------------------------
main :: IO ()
main = do
    putStrLn "Enter number of threads (k):"
    k <- readLn
    setNumCapabilities k  

    putStrLn "Enter array size:"
    n <- readLn

    let !arr = genList n

    putStrLn "\nSorting (Bitonic Sort)..."

    result <- time $ do
        let !sorted = bitonicSort True arr 
        forceList sorted
        return sorted

    putStrLn "\nDone! First 30 elements:"
    print (take 30 result)
