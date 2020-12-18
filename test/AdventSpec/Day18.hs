 {-# LANGUAGE CPP #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}

module AdventSpec.Day18 (spec) where

import Test.Hspec ( describe, it, shouldBe, Spec )
import Advent

spec :: Spec
spec =
  describe "day18" $ do
    it "pt1 test1" $
      day18pt1 ["1 + 2 * 3 + 4 * 5 + 6"] `shouldBe` 71
    it "pt1 test2" $
      day18pt1 ["1 + (2 * 3) + (4 * (5 + 6))"] `shouldBe` 51
    it "pt1 test3" $
      day18pt1 ["2 * 3 + (4 * 5)"] `shouldBe` 26
    it "pt1 test4" $
      day18pt1 ["5 + (8 * 3 + 9 + 3 * 4 * 3)"] `shouldBe` 437
    it "pt1 test5" $
      day18pt1 ["5 * 9 * (7 * 3 * 3 + 9 * 3 + (8 + 6 * 4))"] `shouldBe` 12240
    it "pt1 test6" $
      day18pt1 ["((2 + 4 * 9) * (6 + 9 * 8 + 6) + 6) + 2 + 4 * 2"] `shouldBe` 13632
    it "pt2 test1" $
      day18pt2 ["1 + 2 * 3 + 4 * 5 + 6"] `shouldBe` 231
    it "pt2 test2" $
      day18pt2 ["1 + (2 * 3) + (4 * (5 + 6))"] `shouldBe` 51
    it "pt2 test3" $
      day18pt2 ["2 * 3 + (4 * 5)"] `shouldBe` 46
    it "pt2 test4" $
      day18pt2 ["5 + (8 * 3 + 9 + 3 * 4 * 3)"] `shouldBe` 1445
    it "pt2 test5" $
      day18pt2 ["5 * 9 * (7 * 3 * 3 + 9 * 3 + (8 + 6 * 4))"] `shouldBe` 669060
    it "pt2 test6" $
      day18pt2 ["((2 + 4 * 9) * (6 + 9 * 8 + 6) + 6) + 2 + 4 * 2"] `shouldBe` 23340
