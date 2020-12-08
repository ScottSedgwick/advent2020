module AdventSpec.Day7 (spec) where

import Test.Hspec ( describe, it, shouldBe, Spec )
import Advent 

spec :: Spec
spec = do
  describe "day7" $ do
    it "part1 test" $ do
      xs <- readFile "data/Day7Test.txt"
      day7pt1 xs `shouldBe` 11
    it "part1 actual" $ do
      xs <- readFile "data/Day7Actual.txt"
      day7pt1 xs `shouldBe` 6625
    it "part2 test" $ do
      xs <- readFile "data/Day7Test.txt"
      day7pt2 xs `shouldBe` 6
    it "part2 actual" $ do
      xs <- readFile "data/Day7Actual.txt"
      day7pt2 xs `shouldBe` 3360