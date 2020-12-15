module AdventSpec.Day5 (spec) where

import Test.Hspec ( describe, it, shouldBe, Spec )
import Advent 

spec :: Spec
spec = do
  describe "part1" $ do
    it "part1" $ do
      xs <- parseFile day5parser "data/Day5Actual.txt"
      day5pt1 xs `shouldBe` 922
    it "part2" $ do
      xs <- parseFile day5parser "data/Day5Actual.txt"
      day5pt2 xs `shouldBe` 747
