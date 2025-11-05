import Test.Hspec
import Sort (parallelSort)
import Data.List (sort)

main :: IO ()
main = hspec $ do
  describe "parallelSort" $ do

    it "sorts small lists correctly" $ do
      parallelSort 2 ([5,1,3,2,4] :: [Int]) `shouldBe` sort ([5,1,3,2,4] :: [Int])

    it "works with duplicates" $ do
      parallelSort 3 ([2,2,1,3,1] :: [Int]) `shouldBe` sort ([2,2,1,3,1] :: [Int])

    it "matches standard sort for larger reversed list" $ do
      let xs = ([1000,999..1] :: [Int])
      parallelSort 4 xs `shouldBe` sort xs
