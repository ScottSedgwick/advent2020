module Main (main) where

import Advent

datafile :: FilePath
-- datafile = "data/Day4Test.txt"
-- datafile = "data/Day4Test2.txt"
datafile = "data/Day4Actual.txt"

main :: IO ()
main = do
  p2 <- day4pt2 datafile
  print p2
