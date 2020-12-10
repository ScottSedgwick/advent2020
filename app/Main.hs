module Main (main) where

import Advent

datafile :: FilePath
-- datafile = "data/Day9Test.txt"
datafile = "data/Day7Actual.txt"

main :: IO ()
main = do
  xs <- parseFile day10parser datafile
  x <- process day10pt1 (-1) xs
  print x
  -- let ys = map (\x -> read x :: Integer) $ lines xs
  -- print $ day9pt1 ys
  -- let zs = day9pt2 1930745883 ys
  -- let bs = sort zs
  -- print $ head bs + last bs