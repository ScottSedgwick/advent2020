module Main (main) where

import Advent

datafile :: FilePath
-- datafile = "data/Day8Test.txt"
datafile = "data/Day8Actual.txt"

main :: IO ()
main = do
  xs <- readFile datafile
  print $ day8pt1 xs
  print $ day8pt2 xs