module AdventSpec.Day12 where

import Test.Hspec ( describe, it, shouldBe, Spec )
import Advent 

errcode :: a -> Int
errcode = const (-1)

testfile :: String
testfile = "data/Day12Test.txt"

actualfile :: String
actualfile = "data/Day12Actual.txt"

spec :: Spec
spec = do
  describe "day12" $ do
    it "part1 test" $ do
      xs <- day12parser testfile
      let x = day12pt1 xs 
      x `shouldBe` 25
    it "part1 actual" $ do
      xs <- day12parser actualfile
      let x = day12pt1 xs 
      x `shouldBe` 2458
    it "part2 test" $ do
      xs <- day12parser testfile
      let x = day12pt2 xs 
      x `shouldBe` 286
    it "part2 actual" $ do
      xs <- day12parser actualfile
      let x = day12pt2 xs 
      x `shouldBe` 145117