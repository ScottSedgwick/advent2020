module AdventSpec.Day7 (spec) where

import Test.Hspec ( describe, it, shouldBe, Spec )
import Advent 

spec :: Spec
spec = do
  describe "day6" $ do
    it "part1 test" $ do
      xs <- readFile "data/Day6Test.txt"
      day6pt1 xs `shouldBe` 11
    it "part1 actual" $ do
      xs <- readFile "data/Day6Actual.txt"
      day6pt1 xs `shouldBe` 6625
    it "part2 test" $ do
      xs <- readFile "data/Day6Test.txt"
      day6pt2 xs `shouldBe` 6
    it "part2 actual" $ do
      xs <- readFile "data/Day6Actual.txt"
      day6pt2 xs `shouldBe` 3360