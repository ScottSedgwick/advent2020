module Main (main) where

import Advent

main :: IO ()
main = do
  r <- day11parser "data/Day11Actual.txt"
  print $ day11pt2 r
