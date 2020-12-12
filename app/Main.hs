module Main (main) where

import Advent

main :: IO ()
main = do
  r <- day12parser "data/Day12Actual.txt"
  print $ day12pt1 r
