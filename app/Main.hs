module Main (main) where

import Advent

main :: IO ()
main = do
  d <- parseFile day3parser "data/Day3Test.txt"
  print $ day3pt1 d

