module Main (main) where

import Advent

datafile :: FilePath
-- datafile = "data/Day9Test.txt"
datafile = "data/Day9Actual.txt"

main :: IO ()
main = do
  xs <- parseFile day9parser datafile
  case xs of
    (Left e) -> print e
    (Right ys) -> do
      print $ day9pt1 ys
      print $ day9pt2 1930745883 ys