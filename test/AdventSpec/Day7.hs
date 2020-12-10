module AdventSpec.Day7 (spec) where

import Test.Hspec ( describe, it, shouldBe, Spec )
import Advent 

errcode :: a -> Int
errcode = const (-1)

spec :: Spec
spec = do
  describe "day7" $ do
    it "part1 test" $ do
      xs <- readFile "data/Day7Test.txt"
      day7pt1 xs `shouldBe` 4
    it "part1 actual" $ do
      xs <- readFile "data/Day7Actual.txt"
      day7pt1 xs `shouldBe` 332
    it "part2 test" $ do
      xs <- parseFile day7parser "data/Day7Test2.txt"
      let x = either errcode day7pt2 xs
      x `shouldBe` 126
    it "part2 actual" $ do
      xs <- parseFile day7parser "data/Day7Actual.txt"
      let x = either errcode day7pt2 xs
      x `shouldBe` 10875