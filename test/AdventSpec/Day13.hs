module AdventSpec.Day13 where

import Test.Hspec ( describe, it, shouldBe, Spec )
import Advent

testfile :: String
testfile = "data/Day13Test.txt"

actualfile :: String
actualfile = "data/Day13Actual.txt"

spec :: Spec
spec = do
  describe "day13" $ do
    it "part1 test1" $ do
      xs <- parseFile day13parser testfile
      day13pt1 xs `shouldBe` 295
    it "part1 actual" $ do
      xs <- parseFile day13parser actualfile
      day13pt1 xs `shouldBe` 171
    it "crt test 1" $ do
      crt [(2,7), (0,3), (1,5)] `shouldBe` (51,105)
    it "crt test 2" $ do
      crt [(2,3), (3,4), (1,5)] `shouldBe` (11,60)
    it "crt test 3" $ do
      crt [(0,7), ((-1),13), ((-4),59), ((-6),31), ((-7),19)] `shouldBe` (1068781,3162341)
    it "crt test 4" $ do
      crt [(0,17), ((-2),13), ((-3),19)] `shouldBe` (3417,4199)
    it "part2 test1" $ do
      xs <- parseFile day13parser testfile
      day13pt2 xs `shouldBe` 1068781
    it "part2 actual" $ do
      xs <- parseFile day13parser actualfile
      day13pt2 xs `shouldBe` 539746751134958