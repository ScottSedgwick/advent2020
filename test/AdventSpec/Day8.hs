module AdventSpec.Day8 where

import Test.Hspec ( describe, it, shouldBe, Spec )
import Advent

testfile :: String
testfile = "data/Day8Test.txt"

actualfile :: String
actualfile = "data/Day8Actual.txt"

spec :: Spec
spec = do
  describe "day8" $ do
    it "part1 test1" $ do
      xs <- parseFile day8parser [] testfile
      day8pt1 xs `shouldBe` InfiniteLoop 5
    it "part1 actual" $ do
      xs <- parseFile day8parser [] actualfile
      day8pt1 xs `shouldBe` InfiniteLoop 1087
    it "part2 test1" $ do
      xs <- parseFile day8parser [] testfile
      day8pt2 xs `shouldBe` [Normal 8]
    it "part2 actual" $ do
      xs <- parseFile day8parser [] actualfile
      day8pt2 xs `shouldBe` [Normal 780]