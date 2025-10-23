{-# LANGUAGE BangPatterns #-}
module Main (main) where

import Control.Parallel.Strategies
import Control.DeepSeq (force, NFData)
import System.Random
import Control.Exception (evaluate)
import Data.List (sort, unfoldr)
import System.CPUTime
import Text.Printf

----------------------------------------------------
-- CONFIG
----------------------------------------------------

chunkSizeMin :: Int
chunkSizeMin = 2000  -- безпечний поріг

----------------------------------------------------
-- Classic merge (NOT bitonic network)
----------------------------------------------------

merge :: Ord a => [a] -> [a] -> [a]
merge xs [] = xs
merge [] ys = ys
merge (x:xs) (y:ys)
    | x <= y    = x : merge xs (y:ys)
    | otherwise = y : merge (x:xs) ys

----------------------------------------------------
-- Parallel chunk-based sort + classic merge
----------------------------------------------------

parallelSort :: (NFData a, Ord a) => Int -> [a] -> [a]
parallelSort k xs =
    let parts = chunks k xs
        sortedParts = parMap rdeepseq sort parts
    in foldl1 merge sortedParts

chunks :: Int -> [a] -> [[a]]
chunks n xs =
    let size = max chunkSizeMin (length xs `div` n)
    in unfoldr (\ys ->
        if null ys then Nothing
        else Just $ splitAt size ys) xs

----------------------------------------------------
-- Fast pure random generation
----------------------------------------------------

genList :: Int -> [Int]
genList n = take n $ randomRs (1,1000000) (mkStdGen 42)

----------------------------------------------------
-- Force full evaluation
----------------------------------------------------

forceList :: NFData a => [a] -> IO ()
forceList xs = do
    _ <- evaluate (force xs)
    return ()

----------------------------------------------------
-- Timer
----------------------------------------------------

time :: IO t -> IO t
time a = do
    start <- getCPUTime
    v <- a
    end <- getCPUTime
    let diff = fromIntegral (end - start) / (10 ** 12 :: Double)
    printf "\nExecution time: %.5f sec\n" diff
    return v

----------------------------------------------------
-- MAIN
----------------------------------------------------

main :: IO ()
main = do
    putStrLn "Enter number of threads:"
    k <- readLn

    putStrLn "Enter array size:"
    n <- readLn

    let !arr = genList n

    putStrLn "\nSorting..."
    result <- time $ do
        let !r = parallelSort k arr
        forceList r
        return r

    putStrLn "\nDone! First 30 elements:"
    print (take 30 result)
