{-# LANGUAGE DerivingStrategies #-}
module Advent.Day9
  ( day9pt1
  , day9pt2
  ) where

import Data.List (find, inits)
import Text.Read (readMaybe)

day9pt1 :: [Integer] -> Integer
day9pt1 xs = firstBad (take 25 xs) (drop 25 xs)

firstBad :: [Integer] -> [Integer] -> Integer
firstBad _ [] = -1
firstBad xs (y:ys) =
  if isGood y xs
    then firstBad (tail xs <> [y]) ys
    else y

isGood :: Integer -> [Integer] -> Bool
isGood x xs = any (\(a,b) -> a+b == x) (pairs xs)

pairs :: [Integer] -> [(Integer,Integer)]
pairs [] = []
pairs (x:xs) = p x xs <> pairs xs
  where
    p _ [] = []
    p y (z:zs) = (y,z):(p y zs)

day9pt2 :: Integer -> [Integer] -> [Integer]
day9pt2 x [] = []
day9pt2 x xs = 
  case f x (inits xs) of
    Nothing -> day9pt2 x (tail xs)
    (Just ys) -> ys
  
f :: Integer -> [[Integer]] -> Maybe [Integer]
f x xs = find (\y -> x == sum y) xs

-- find :: Foldable t => (a -> Bool) -> t a -> Maybe aSource