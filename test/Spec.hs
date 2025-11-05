import Test.Hspec
import Sort (bitonicSort)
import Data.List (sort)

main :: IO ()
main = hspec $ do
  describe "bitonicSort" $ do

    it "sorts small list correctly" $ do
      bitonicSort True ([5,1,3,2,4] :: [Int])
        `shouldBe` sort ([5,1,3,2,4] :: [Int])

    it "handles duplicates" $ do
      bitonicSort True ([2,2,1,3,1] :: [Int])
        `shouldBe` sort ([2,2,1,3,1] :: [Int])

    it "matches standard sort for large reversed list" $ do
      let xs = ([1000,999..1] :: [Int])
      bitonicSort True xs `shouldBe` sort xs
