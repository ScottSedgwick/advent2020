module AdventSpec.Day15 where

import Test.Hspec ( describe, it, shouldBe, Spec )
import Advent

spec :: Spec
spec = do
  describe "day14" $ do
    it "part1 test1" $ do
      day15pt1 day15test `shouldBe` 436
    it "part1 actual" $ do
      day15pt1 day15actual `shouldBe` 1522
    it "part2 test1" $ do
      day15pt2 day15test `shouldBe` 175594
    it "part2 actual" $ do
      day15pt2 day15actual `shouldBe` 18234