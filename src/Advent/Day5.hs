module Advent.Day5
  ( day5pt1
  , day5pt2
  , seatFrom
  ) where

import Data.Char (digitToInt)
import Data.List (foldl', sort)

day5pt1 :: String -> [Int]
day5pt1 xs = reverse $ sort $ map seatFrom (lines xs)

seatFrom :: String -> Int
seatFrom xs = binToDec (replace 'F' '0' $ replace 'B' '1' $ take 7 xs) * 8 + binToDec (replace 'R' '1' $ replace 'L' '0' $ drop 7 xs)

binToDec :: String -> Int
binToDec = foldl' (\acc x -> acc * 2 + digitToInt x) 0

replace :: Eq a => a -> a -> [a] -> [a]
replace a b = map (\x -> if x == a then b else x)

day5pt2 :: String -> Int
day5pt2 xs = findGap ys (tail ys)
  where
    ys = day5pt1 xs

findGap :: [Int] -> [Int] -> Int
findGap (x:xs) (y:ys) = 
  if x - y == 1 
    then findGap xs ys 
    else x - 1
findGap _ _ = -1