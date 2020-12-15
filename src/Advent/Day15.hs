module Advent.Day15
  ( day15pt1
  , day15pt2
  , day15test
  , day15actual
  ) where

import qualified Data.IntMap as IM

data Day15 = Day15
  { lastNum :: Int
  , prevNums :: IM.IntMap Int
  } deriving stock (Show)

day15test :: [Int]
day15test = [6,3,0]

day15actual :: [Int]
day15actual = reverse [9,19,1,6,0,5,4]

day15pt1 :: [Int] -> Int
day15pt1 xs = pt1 2020 (length xs) (Day15 { lastNum = head xs, prevNums = IM.fromList (zip (reverse (tail xs)) [0..])})

pt1 :: Int -> Int -> Day15 -> Int
pt1 l n d | l == n = lastNum d
          | otherwise = pt1 l (n + 1) (Day15 { lastNum = ln', prevNums = pn'})
  where
    ln' = case IM.lookup (lastNum d) (prevNums d) of
            Nothing -> 0
            (Just lt) -> n - lt -1
    pn' = IM.insert (lastNum d) (n - 1) (prevNums d)

day15pt2 :: [Int] -> Int
day15pt2 xs = pt1 30000000 (length xs) (Day15 { lastNum = head xs, prevNums = IM.fromList (zip (reverse (tail xs)) [0..])})
