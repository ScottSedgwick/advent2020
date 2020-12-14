module AdventSpec.Day2 (spec) where

import Test.Hspec
import Advent

process' :: ([Pwd] -> IO()) -> FilePath -> IO()
process' f fname = do 
  ps <- parseFile day2Parser fname
  f ps 

spec :: Spec
spec = do
  describe "part1" $ do
    it "test data should be correct" $ 
      process' (\ps -> day2ValidCount day2IsValid1 ps `shouldBe` 2) "data/Day2Test.txt"
    it "actual data should be correct" $ 
      process' (\ps -> day2ValidCount day2IsValid1 ps `shouldBe` 550) "data/Day2Actual.txt"
  describe "part2" $ do
    it "test data should be correct" $ 
      process' (\ps -> day2ValidCount day2IsValid2 ps `shouldBe` 1) "data/Day2Test.txt"
    it "actual data should be correct" $ 
      process' (\ps -> day2ValidCount day2IsValid2 ps `shouldBe` 634) "data/Day2Actual.txt"