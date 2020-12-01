module AdventSpec.Day1 (spec) where

import Test.Hspec
import Advent

spec :: Spec
spec = do
  describe "part1" $ do
    it "test data should be correct" $ 
      day1pt1 day1TestData `shouldBe` 514579
    it "actual data should be correct" $ 
      day1pt1 day1ActualData `shouldBe` 437931
  describe "part2" $ do
    it "test data should be correct" $ 
      day1pt2 day1TestData `shouldBe` 241861950
    it "actual data should be correct" $ 
      day1pt2 day1ActualData `shouldBe` 157667328