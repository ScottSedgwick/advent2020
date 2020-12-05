module AdventSpec.Day6 (spec) where

import Test.Hspec ( describe, it, shouldBe, Spec )
import Advent 

spec :: Spec
spec = do
  describe "part1" $ do
    it "test data should be correct" $ do
      seatFrom "FBFBBFFRLR" `shouldBe` (44,5)
      seatFrom "BFFFBBFRRR" `shouldBe` (70,7)
      seatFrom "FFFBBBFRRR" `shouldBe` (14,7)
      seatFrom "BBFFBBFRLL" `shouldBe` (102,4)
    it "part1" $ do
      xs <- readFile "data/Day5Actual.txt"
      let ys = day5pt1 xs
      head ys `shouldBe` 922
    it "part2" $ do
      xs <- readFile "data/Day5Actual.txt"
      let ys = day5pt2 xs
      ys `shouldBe` 747