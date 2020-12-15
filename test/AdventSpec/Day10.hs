module AdventSpec.Day10 where

import Test.Hspec ( describe, it, shouldBe, Spec )
import Advent

testfile1 :: String
testfile1 = "data/Day10Test1.txt"

testfile2 :: String
testfile2 = "data/Day10Test2.txt"

actualfile :: String
actualfile = "data/Day10Actual.txt"

spec :: Spec
spec = do
  describe "day10" $ do
    it "part1 test1" $ do
      xs <- parseFile day10parser testfile1
      day10pt1 xs `shouldBe` 35
    it "part1 test2" $ do
      xs <- parseFile day10parser testfile2
      day10pt1 xs `shouldBe` 220
    it "part1 actual" $ do
      xs <- parseFile day10parser actualfile
      day10pt1 xs `shouldBe` 2210
    it "part2 test1" $ do
      xs <- parseFile day10parser testfile1
      day10pt2 xs `shouldBe` 8
    it "part2 test2" $ do
      xs <- parseFile day10parser testfile2
      day10pt2 xs `shouldBe` 19208
    it "part2 actual" $ do
      xs <- parseFile day10parser actualfile
      day10pt2 xs `shouldBe` 7086739046912
