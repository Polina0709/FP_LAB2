{-# LANGUAGE ScopedTypeVariables #-}

import Test.Hspec
import Sort (bitonicSort, bitonicSortParallel)
import System.Random (mkStdGen, randomRs)
import Data.List (sort)

main :: IO ()
main = hspec $ do

  ----------------------------------------------------
  -- SEQUENTIAL TESTS
  ----------------------------------------------------
  describe "bitonicSort (sequential)" $ do

    it "sorts a known fixed list" $ do
      let input    :: [Int]
          input    = [5,1,3,2,4]
      let expected :: [Int]
          expected = [1,2,3,4,5]
      bitonicSort input `shouldBe` expected

    it "handles duplicated values correctly" $ do
      let input    :: [Int]
          input    = [4,2,4,1,2]
      let expected :: [Int]
          expected = [1,2,2,4,4]
      bitonicSort input `shouldBe` expected

  ----------------------------------------------------
  -- PARALLEL TESTS
  ----------------------------------------------------
  describe "bitonicSortParallel" $ do

    it "matches sequential on moderately large random input" $ do
      let gen = mkStdGen 999
      let input :: [Int]
          input = take 20000 (randomRs (1, 1000000) gen)
      bitonicSortParallel 4 input `shouldBe` bitonicSort input

    it "produces sorted result on a large reversed list (100k elements)" $ do
      let input :: [Int]
          input = reverse [1..100000]
      let result = bitonicSortParallel 4 input
      result `shouldBe` sort input

    it "works with different thread counts (medium size)" $ do
      let input :: [Int]
          input = reverse [1..30000]
      let seqSorted = bitonicSort input
      bitonicSortParallel 2 input `shouldBe` seqSorted
      bitonicSortParallel 4 input `shouldBe` seqSorted
      bitonicSortParallel 8 input `shouldBe` seqSorted
