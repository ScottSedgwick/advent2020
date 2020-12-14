module Main (main) where

import Advent

main :: IO ()
main = do
  d <- parseFile day13parser "data/Day13Actual.txt"
  print $ day13pt2 d

