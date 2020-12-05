module AdventSpec.Day5 (spec) where

import Test.Hspec ( describe, it, shouldBe, Spec )
import Advent 

spec :: Spec
spec = do
  describe "part1" $ do
    it "test data should be correct" $ do
      x <- day4pt1 "data/Day4Test.txt"
      x `shouldBe` 2
    it "actual data should be correct" $ do
      x <- day4pt2 "data/Day4Test2.txt"
      x `shouldBe` 4
  describe "part2" $ do
    it "test data should be correct" $ do
      x <- day4pt1 "data/Day4Actual.txt"
      x `shouldBe` 213
    it "actual data should be correct" $ do
      x <- day4pt2 "data/Day4Actual.txt"
      x `shouldBe` 147