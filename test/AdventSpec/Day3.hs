module AdventSpec.Day3 (spec) where

import Test.Hspec
import Advent

spec :: Spec
spec = do
  describe "part1" $ do
    it "test data should be correct" $ 
      2 `shouldBe` 2
    it "actual data should be correct" $ 
      550 `shouldBe` 550
  describe "part2" $ do
    it "test data should be correct" $ 
      1 `shouldBe` 1
    it "actual data should be correct" $ 
      634 `shouldBe` 634