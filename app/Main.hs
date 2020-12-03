module Main (main) where

import Advent

datafile :: FilePath
-- datafile = "data/Day3Test.txt"
datafile = "data/Day3Actual.txt"

main :: IO ()
main = do
  xs <- day3loader datafile
  print $ day3pt2 xs
