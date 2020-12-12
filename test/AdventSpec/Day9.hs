module AdventSpec.Day9 where

import Test.Hspec ( describe, it, shouldBe, Spec )
import Advent 

errcode :: a -> Int
errcode = const (-1)

testfile :: FilePath
testfile = "data/Day9Test.txt"

actualfile :: FilePath
actualfile = "data/Day9Actual.txt"

type TestData = [Int]

parser :: FilePath -> IO TestData
parser = day9parser

pt1 :: TestData -> Int
pt1 = day9pt1

pt2 :: Int -> TestData -> Int
pt2 = day9pt2

spec :: Spec
spec = do
  describe "day9" $ do
    it "part1 test" $ do
      xs <- parser testfile
      let x = pt1 xs 
      x `shouldBe` 64
    it "part1 actual" $ do
      xs <- parser actualfile
      let x = pt1 xs 
      x `shouldBe` 1930745883
    it "part2 test" $ do
      xs <- parser testfile
      let x = pt2 64 xs 
      x `shouldBe` 128
    it "part2 actual" $ do
      xs <- parser actualfile
      let x = pt2 1930745883 xs 
      x `shouldBe` 268878261