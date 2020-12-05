module Main (main) where

import Advent

datafile :: FilePath
datafile = "data/Day5Actual.txt"

main :: IO ()
main = do
  xs <- readFile datafile
  print $ head (day5pt1 xs)
  print $ day5pt2 xs
