module Main (main) where

import Advent

datafile :: FilePath
-- datafile = "data/Day6Test.txt"
datafile = "data/Day6Actual.txt"

main :: IO ()
main = do
  xs <- readFile datafile
  print $ day6pt2 xs