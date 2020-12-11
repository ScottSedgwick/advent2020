module Advent.Day10
  ( day10pt1
  , day10pt2
  ) where

import Data.List (sort)

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
day10pt2 xs = head $ foldr f [1] [0..length n-2]
  where
    f i acc = sum [acc !! (j-i-1) | j <- [i+1..i+3], j < length n, (n !! j) - (n !! i) <= 3] : acc
    n = sort (maximum xs + 3 : 0 : xs)