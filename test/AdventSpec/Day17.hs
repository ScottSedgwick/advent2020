module AdventSpec.Day17 where

import Test.Hspec ( describe, it, shouldBe, Spec )
import Advent

testfile :: FilePath
testfile = "data/Day17Test.txt"

actualfile :: FilePath
actualfile = "data/Day17Actual.txt"

spec :: Spec
spec = do
  describe "day17" $ do
    it "part1 test1" $ do
      xs <- parseFile day17parser testfile
      day17pt1 xs `shouldBe` 112
    it "part1 actual" $ do
      xs <- parseFile day17parser actualfile
      day17pt1 xs `shouldBe` 315
    it "part2 test1" $ do
      xs <- parseFile day17parser testfile
      day17pt2 xs `shouldBe` 848
    it "part2 actual" $ do
      xs <- parseFile day17parser actualfile
      day17pt2 xs `shouldBe` 1520