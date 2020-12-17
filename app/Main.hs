module Main (main) where

import Advent

filename :: FilePath
-- filename = "data/Day17Test.txt"
filename = "data/Day17Actual.txt"

main :: IO ()
main = do
  xs <- parseFile day17parser filename
  -- print $ day17pt1 xs
  print $ day17pt2 xs

