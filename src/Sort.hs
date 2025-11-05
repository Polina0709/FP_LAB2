{-# LANGUAGE BangPatterns #-}
module Sort (parallelSort, merge, genList) where

import Control.Parallel.Strategies
import Control.DeepSeq (NFData)
import System.Random
import Data.List (sort, unfoldr)

chunkSizeMin :: Int
chunkSizeMin = 2000

merge :: Ord a => [a] -> [a] -> [a]
merge xs [] = xs
merge [] ys = ys
merge (x:xs) (y:ys)
    | x <= y    = x : merge xs (y:ys)
    | otherwise = y : merge (x:xs) ys

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

genList :: Int -> [Int]
genList n = take n $ randomRs (1,1000000) (mkStdGen 42)
