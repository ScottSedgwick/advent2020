module Advent.Day10
  ( day10pt1
  , day10pt2
  , day10parser
  ) where

import Control.Lens
import Data.List (sort)
import Data.Maybe (catMaybes)
import Advent.ParseUtils
import Text.Megaparsec

day10parser :: Parser [Int]
day10parser = some intline

day10pt1 :: [Int] -> Int
day10pt1 = f 0 0 1 . sort
  where
    f _ ones threes [] = ones * threes
    f input ones threes (x:xs) = f input' ones' threes' xs
      where
        input' = x
        ones' = if (x - input) == 1 then ones + 1 else ones
        threes' = if (x - input) == 3 then threes + 1 else threes

day10pt2 :: [Int] -> Int
day10pt2 xs = head $ foldr f [1] [0..length ys-2]
  where
    ys = sort (0 : maximum xs + 3 : xs)
    f i a = sum (catMaybes [a ^? element (j - i - 1) | j <- [i + 1 .. i + 3], isValid (ys ^? element j) (ys ^? element i)]) : a
    isValid (Just x) (Just y) = x - y <= 3
    isValid _ _ = False