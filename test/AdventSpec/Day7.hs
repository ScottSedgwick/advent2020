module AdventSpec.Day7 (spec) where

import Test.Hspec ( describe, it, shouldBe, Spec )
import Advent 

spec :: Spec
spec = do
  describe "day7" $ do
    it "part1 test" $ do
      xs <- parseFile day7parser [] "data/Day7Test.txt"
      day7pt1 xs `shouldBe` 4
    it "part1 actual" $ do
      xs <- parseFile day7parser [] "data/Day7Actual.txt"
      day7pt1 xs `shouldBe` 332
    it "part2 test" $ do
      xs <- parseFile day7parser [] "data/Day7Test.txt"
      day7pt2 xs `shouldBe` 32
    it "part2 test" $ do
      xs <- parseFile day7parser [] "data/Day7Test2.txt"
      day7pt2 xs `shouldBe` 126
    it "part2 actual" $ do
      xs <- parseFile day7parser [] "data/Day7Actual.txt"
      day7pt2 xs `shouldBe` 10875