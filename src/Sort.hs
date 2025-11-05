module Sort (bitonicSort, bitonicSortParallel, genList) where

import Control.Parallel.Strategies
import System.Random
import Data.List (sort)
import Control.DeepSeq (NFData)
import GHC.Conc (numCapabilities)

----------------------------------------------------
-- Generate list
----------------------------------------------------

genList :: Int -> [Int]
genList n = take n $ randomRs (1, 1000000) (mkStdGen 42)

----------------------------------------------------
-- Pad list to next power of two
----------------------------------------------------

nextPow2 :: Int -> Int
nextPow2 n
  | n <= 1    = 1
  | otherwise = head $ dropWhile (< n) (iterate (*2) 1)

padToPow2 :: (Bounded a) => [a] -> [a]
padToPow2 xs =
    let target = nextPow2 (length xs)
        padCount = target - length xs
    in xs ++ replicate padCount maxBound

----------------------------------------------------
-- Public: Sequential Bitonic Sort
----------------------------------------------------

bitonicSort :: (Ord a, NFData a, Bounded a) => [a] -> [a]
bitonicSort xs =
    let padded = padToPow2 xs
        sorted = bitonic padded True
    in take (length xs) sorted

----------------------------------------------------
-- Public: Parallel Bitonic Sort using k threads
----------------------------------------------------

bitonicSortParallel :: (Ord a, NFData a, Bounded a) => Int -> [a] -> [a]
bitonicSortParallel k xs =
    let chunksList = chunk k xs
        sortedChunks = parMap rdeepseq bitonicSort chunksList
    in mergeAll sortedChunks

chunk :: Int -> [a] -> [[a]]
chunk k xs =
    let n = length xs
        size = max 1 (n `div` k)
    in takeWhile (not . null) (map (take size) (iterate (drop size) xs))

mergeAll :: (Ord a) => [[a]] -> [a]
mergeAll = foldl1 merge

merge :: Ord a => [a] -> [a] -> [a]
merge xs [] = xs
merge [] ys = ys
merge (x:xs) (y:ys)
    | x <= y    = x : merge xs (y:ys)
    | otherwise = y : merge (x:xs) ys

----------------------------------------------------
-- Internal Bitonic Kernel
----------------------------------------------------

bitonic :: (Ord a, NFData a) => [a] -> Bool -> [a]
bitonic [] _  = []
bitonic [x] _ = [x]
bitonic xs up =
    let (first, second) = splitAt (length xs `div` 2) xs
        sorted1 = bitonic first True
        sorted2 = bitonic second False
    in bitonicMerge up (sorted1 ++ sorted2) `using` parList rdeepseq

bitonicMerge :: (Ord a) => Bool -> [a] -> [a]
bitonicMerge _ []  = []
bitonicMerge _ [x] = [x]
bitonicMerge up xs =
    let (left, right) = bitonicSplit up xs
    in bitonicMerge up left ++ bitonicMerge up right

bitonicSplit :: (Ord a) => Bool -> [a] -> ([a], [a])
bitonicSplit up xs =
    let half = length xs `div` 2
        first  = take half xs
        second = drop half xs
        cmp a b = if (a <= b) == up then a else b
        cmp' a b = if (a <= b) == up then b else a
        left  = zipWith cmp  first second
        right = zipWith cmp' first second
    in (left, right)
