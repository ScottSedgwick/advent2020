module Main (main) where

import Advent

filename :: FilePath
filename = "data/Day18Actual.txt"

main :: IO ()
main = do
  xs <- readFile filename
  print $ day18pt1 (lines xs)
  print $ day18pt2 (lines xs)


