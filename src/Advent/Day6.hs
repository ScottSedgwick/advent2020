module Advent.Day6
  ( day6pt1
  , day6pt2
  ) where

import Data.Char (isAlpha)
import Data.List(nub,sort, group)
import Data.List.Split (splitOn)

day6pt1 :: String -> Int
day6pt1 = splitAndProcess score

day6pt2 :: String -> Int
day6pt2 = splitAndProcess score2

splitAndProcess :: (String -> Int) -> String -> Int
splitAndProcess f = sum . map f . splitOn "\n\n"

score :: String -> Int
score = length . nub . filter isAlpha

score2 :: String -> Int
score2 xs = length (filter (\b -> length b == length (lines xs)) (group $ sort xs))