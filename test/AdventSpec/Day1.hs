module AdventSpec.Day1 (spec) where

import Test.Hspec
import Advent

spec :: Spec
spec = do
  describe "part1" $ do
    it "test data should be correct" $ do
      xs <- parseFile day1parser "data/Day1Test.txt"
      day1pt1 xs `shouldBe` 514579
    it "actual data should be correct" $ do
      xs <- parseFile day1parser "data/Day1Actual.txt"
      day1pt1 xs `shouldBe` 437931
  describe "part2" $ do
    it "test data should be correct" $ do
      xs <- parseFile day1parser "data/Day1Test.txt"
      day1pt2 xs `shouldBe` 241861950
    it "actual data should be correct" $ do
      xs <- parseFile day1parser "data/Day1Actual.txt"
      day1pt2 xs `shouldBe` 157667328