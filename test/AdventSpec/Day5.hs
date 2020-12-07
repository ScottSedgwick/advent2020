module AdventSpec.Day5 (spec) where

import Test.Hspec ( describe, it, shouldBe, Spec )
import Advent 

spec :: Spec
spec = do
  describe "part1" $ do
    it "part1" $ do
      xs <- readFile "data/Day5Actual.txt"
      let ys = day5pt1 xs
      head ys `shouldBe` 922
    it "part2" $ do
      xs <- readFile "data/Day5Actual.txt"
      let ys = day5pt2 xs
      ys `shouldBe` 747
