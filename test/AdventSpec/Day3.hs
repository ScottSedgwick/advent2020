module AdventSpec.Day3 (spec) where

import Test.Hspec
import Advent

spec :: Spec
spec = do
  describe "part1" $ do
    it "test data should be correct" $ 
      day2ValidCount day2IsValid1 day2TestData `shouldBe` 2
    it "actual data should be correct" $ 
      day2ValidCount day2IsValid1 day2ActualData `shouldBe` 550
  describe "part2" $ do
    it "test data should be correct" $ 
      day2ValidCount day2IsValid2 day2TestData `shouldBe` 1
    it "actual data should be correct" $ 
      day2ValidCount day2IsValid2 day2ActualData `shouldBe` 634