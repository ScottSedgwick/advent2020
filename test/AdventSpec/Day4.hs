module AdventSpec.Day4 (spec) where

import Test.Hspec ( describe, it, shouldBe, Spec )
import Advent ( day3loader, day3pt1, day3pt2 )

spec :: Spec
spec = do
  describe "part1" $ do
    it "test data should be correct" $ do
      xs <- day3loader "data/Day3Test.txt"
      day3pt1 xs `shouldBe` 7
    it "actual data should be correct" $ do
      xs <- day3loader "data/Day3Actual.txt"
      day3pt1 xs `shouldBe` 228
  describe "part2" $ do
    it "test data should be correct" $ do
      xs <- day3loader "data/Day3Test.txt"
      day3pt2 xs `shouldBe` 336
    it "actual data should be correct" $ do
      xs <- day3loader "data/Day3Actual.txt"
      day3pt2 xs `shouldBe` 6818112000