module Main (main) where

import Advent

main :: IO ()
main = do
  r <- day9parser "data/Day9Actual.txt"
  print $ day9pt1 r
