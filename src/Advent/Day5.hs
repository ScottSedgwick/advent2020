module Advent.Day5
  ( day5pt1
  , day5pt2
  , seatFrom
  ) where

import Data.List (sort)

day5pt1 :: String -> [Int]
day5pt1 xs = reverse $ sort $ map (getId . seatFrom) (lines xs)

getId :: (Int, Int) -> Int
getId (a,b) = a * 8 + b

seatFrom :: String -> (Int, Int)
seatFrom xs = (binSearch 0 127 (take 7 xs), binSearch 0 7 (drop 7 xs))

binSearch :: Int -> Int -> String -> Int
binSearch minVal maxVal (x:xs) | x `elem` "FL" = binSearch minVal ((maxVal + minVal) `div` 2) xs
                               | x `elem` "BR" = binSearch (((maxVal + minVal) `div` 2) + 1) maxVal xs
binSearch minVal _ _ = minVal

day5pt2 :: String -> Int
day5pt2 xs = findGap ys (tail ys)
  where
    ys = day5pt1 xs

findGap :: [Int] -> [Int] -> Int
findGap [] _ = -1
findGap _ [] = -1
findGap (x:xs) (y:ys) = 
  if x - y == 1 
    then findGap xs ys 
    else x - 1