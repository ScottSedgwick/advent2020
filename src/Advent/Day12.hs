module Advent.Day12
  ( day12parser
  , day12pt1
  , day12pt2
  ) where

data Dirn = East | West | North | South deriving stock (Show, Eq)

day12parser :: FilePath -> IO [String]
day12parser f = do
  xs <- readFile f
  pure $ lines xs

day12pt1 :: [String] -> Int
day12pt1 xs = pt1 0 0 East xs

pt1 :: Int -> Int -> Dirn -> [String] -> Int
pt1 x y _ []     = abs x + abs y
pt1 x y d (a:xs) = pt1 (x + dx1 d a) (y + dy1 d a) (dd1 d a) xs

dx1 :: Dirn -> String -> Int
dx1 d (a:xs) = 
  case a of
    'N' -> dist
    'S' -> negate dist
    'F' -> case d of
            North -> dist
            South -> negate dist
            _ -> 0
    _   -> 0
  where
    dist = read xs :: Int
dx1 _ [] = 0

dy1 :: Dirn -> String -> Int
dy1 d (a:xs) = 
  case a of
    'E' -> read xs :: Int
    'W' -> (-1) * read xs :: Int
    'F' -> case d of
            East -> read xs :: Int
            West -> (-1) * read xs :: Int
            _ -> 0
    _   -> 0
dy1 _ [] = 0

dd1 :: Dirn -> String -> Dirn
dd1 d (a:xs) = 
  case a of
    'L' -> case d of
            North -> if x == 90 then West  else if x == 180 then South else East
            South -> if x == 90 then East  else if x == 180 then North else West
            East  -> if x == 90 then North else if x == 180 then West  else South
            West  -> if x == 90 then South else if x == 180 then East  else North
    'R' -> case d of
            North -> if x == 90 then East  else if x == 180 then South else West
            South -> if x == 90 then West  else if x == 180 then North else East
            East  -> if x == 90 then South else if x == 180 then West  else North
            West  -> if x == 90 then North else if x == 180 then East  else South
    _   -> d
  where x = read xs :: Int
dd1 d [] = d

day12pt2 :: [String] -> Int
day12pt2 xs = pt2 (10,1) (0, 0) xs

pt2 :: (Int, Int) -> (Int, Int) -> [String] -> Int
pt2 _ (x, y) [] = abs x + abs y
pt2 d p (a:xs)  = pt2 d' p' xs
  where
    d' = dd2 d a
    p' = dp2 d p a

dd2 :: (Int, Int) -> String -> (Int, Int)
dd2 p [] = p
dd2 (dx, dy) (a:xs) = 
  case a of
    'N' -> (dx, dy + dist) 
    'S' -> (dx, dy - dist) 
    'E' -> (dx + dist, dy) 
    'W' -> (dx - dist, dy) 
    'R' -> rotate dist (dx, dy)
    'L' -> rotate (neg dist) (dx, dy)
    _   -> (dx, dy)
  where
    dist = read xs :: Int
    neg x | x == 90   = 270
          | x == 270  = 90
          | otherwise = x
    rotate d (x,y) | d == 90   = (y, negate x)
                   | d == 180  = (negate x, negate y)
                   | d == 270  = (negate y, x)
                   | otherwise = (x, y)

dp2 :: (Int, Int) -> (Int, Int) -> String -> (Int, Int)
dp2 (dx, dy) (x, y) ('F':xs) = (x + dx * m, y + dy * m)
  where
    m = read xs :: Int
dp2 _ p _ = p