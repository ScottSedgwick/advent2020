module Advent.Day1
  ( day1parser
  , day1pt1
  , day1pt2
  ) where

import Safe (headMay)
import Advent.ParseUtils
import Text.Megaparsec
-- import Text.Megaparsec.Char

day1parser :: Parser [Int]
day1parser = some intline

safeHead :: [Int] -> Int
safeHead xs = maybe 0 id $ headMay xs

day1pt1 :: [Int] -> Int
day1pt1 xs = safeHead [a*b | a <- xs, b <- xs, a + b == 2020]

day1pt2 :: [Int] -> Int
day1pt2 xs = safeHead [a*b*c | a <- xs, b <- xs, c <- xs, a + b + c == 2020] 
