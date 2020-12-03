module Advent.Day3
  ( day3loader
  , day3pt1
  , day3pt2
  ) where

day3loader :: FilePath -> IO [[Int]]
day3loader fpath = do
  xs <- readFile fpath
  let ys = lines xs
  pure $ map f ys
  where
    f cs = map g cs
    g '#' = 1
    g _   = 0

runSlope :: [[Int]] -> (Int, Int) -> Int
runSlope xs (dx, dy) = f 0 0
  where
    f x y = if y >= length xs 
            then 0
            else g x (xs !! y) + f (x + dx) (y + dy) 
    g x ys = ys !! (x `mod` length ys) 

day3pt1 :: [[Int]] -> Int
day3pt1 xs = runSlope xs (3, 1)

day3pt2 :: [[Int]] -> Int
day3pt2 xs = product $ map (runSlope xs) [(1,1), (3,1), (5,1), (7,1), (1,2)]
