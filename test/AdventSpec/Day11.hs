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
    it "cycleOnce Test 1" $ do
      f "data/Day11Test.txt" "data/Day11Test2.txt"
    it "cycleOnce Test 2" $ do
      f "data/Day11Test2.txt" "data/Day11Test3.txt"
    it "part1 test" $ do
      xs <- day11parser testfile
      let x = day11pt1 xs 
      x `shouldBe` 37
    it "part1 actual" $ do
      xs <- day11parser actualfile
      let x = day11pt1 xs 
      x `shouldBe` 2152
    it "part2 test" $ do
      xs <- day11parser testfile
      let x = day11pt2 xs 
      x `shouldBe` 26
    it "part2 actual" $ do
      xs <- day11parser actualfile
      let x = day11pt2 xs 
      x `shouldBe` 1937

f :: FilePath -> FilePath -> IO()
f f1 f2 = do
  r0 <- day11parser f1
  r1 <- day11parser f2
  (cycleOnce countAdjacent 4 r0) `shouldBe` r1
