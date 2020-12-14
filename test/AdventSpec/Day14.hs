module AdventSpec.Day14 where

import Test.Hspec ( describe, it, shouldBe, Spec )
import Advent

testfile1 :: String
testfile1 = "data/Day14Test.txt"

testfile2 :: String
testfile2 = "data/Day14Test2.txt"

actualfile :: String
actualfile = "data/Day14Actual.txt"

spec :: Spec
spec = do
  describe "day14" $ do
    it "part1 test1" $ do
      xs <- parseFile day14parser testfile1
      day14pt1 xs `shouldBe` 165
    it "part1 actual" $ do
      xs <- parseFile day14parser actualfile
      day14pt1 xs `shouldBe` 15018100062885
    it "part2 test1" $ do
      xs <- parseFile day14parser testfile2
      day14pt2 xs `shouldBe` 208
    it "part2 actual" $ do
      xs <- parseFile day14parser actualfile
      day14pt2 xs `shouldBe` 5724245857696