module Main (main) where

import Advent

main :: IO ()
main = do
  d <- parseFile day1parser "data/Day1Test.txt"
  print $ day1pt2 d

