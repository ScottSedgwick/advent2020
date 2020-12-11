module AdventSpec.Day10 where

import Test.Hspec ( describe, it, shouldBe, Spec )
import Advent 

errcode :: a -> Int
errcode = const (-1)

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
      xs <- parseFile ints testfile1
      let x = either errcode day10pt1 xs
      x `shouldBe` 35
    it "part1 test2" $ do
      xs <- parseFile ints testfile2
      let x = either errcode day10pt1 xs
      x `shouldBe` 220
    it "part1 actual" $ do
      xs <- parseFile ints actualfile
      let x = either errcode day10pt1 xs 
      x `shouldBe` 2210
    it "part2 test1" $ do
      xs <- parseFile ints testfile1
      let x = either errcode day10pt2 xs
      x `shouldBe` 8
    it "part2 test2" $ do
      xs <- parseFile ints testfile2
      let x = either errcode day10pt2 xs
      x `shouldBe` 19208
    it "part2 actual" $ do
      xs <- parseFile ints actualfile
      let x = either errcode day10pt2 xs
      x `shouldBe` 7086739046912
