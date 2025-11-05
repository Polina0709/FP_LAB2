{-# LANGUAGE BangPatterns #-}
module Main (main) where

import Sort (parallelSort, genList)
import Control.DeepSeq (NFData, force)
import Control.Exception (evaluate)
import System.CPUTime
import Text.Printf

----------------------------------------------------
-- Force evaluation
----------------------------------------------------

forceList :: NFData a => [a] -> IO ()
forceList xs = do
    _ <- evaluate (force xs)
    return ()

----------------------------------------------------
-- Timer
----------------------------------------------------

time :: IO t -> IO t
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
    putStrLn "Enter number of threads:"
    k <- readLn     -- Number of parallel chunks

    putStrLn "Enter array size:"
    n <- readLn     -- Array size

    let !arr = genList n

    putStrLn "\nSorting..."
    result <- time $ do
        let !r = parallelSort k arr
        forceList r
        return r

    putStrLn "\nDone! First 30 elements:"
    print (take 30 result)
