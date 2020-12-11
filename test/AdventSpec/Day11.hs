module AdventSpec.Day11 where

import Test.Hspec ( describe, it, shouldBe, Spec )
import Advent 

errcode :: a -> Int
errcode = const (-1)

testfile :: String
testfile = "data/Day11Test.txt"

actualfile :: String
actualfile = "data/Day11Actual.txt"

spec :: Spec
spec = do
  describe "day11" $ do
    it "part1 test" $ do
      xs <- parseFile ints testfile
      let x = either errcode day11pt1 xs
      x `shouldBe` 35
    it "part1 actual" $ do
      xs <- parseFile ints actualfile
      let x = either errcode day11pt1 xs 
      x `shouldBe` 2210
    it "part2 test" $ do
      xs <- parseFile ints testfile
      let x = either errcode day11pt2 xs
      x `shouldBe` 8
    it "part2 actual" $ do
      xs <- parseFile ints actualfile
      let x = either errcode day11pt2 xs
      x `shouldBe` 7086739046912
