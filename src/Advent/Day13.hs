module Advent.Day13
  ( day13parser,
    day13pt1,
    day13pt2,
    crt,
  )
where

import Advent.ParseUtils
import Data.Bifunctor (first)
import Data.List (sortOn)
import Text.Megaparsec

data BusSched = BusSched
  { departTime :: Integer,
    buses :: [(Integer, Integer)]
  }
  deriving stock (Show, Eq)

instance Semigroup BusSched where
  (<>) a b =
    BusSched
      { departTime = departTime a + departTime b,
        buses = buses a <> buses b
      }

instance Monoid BusSched where
  mempty =
    BusSched
      { departTime = 0,
        buses = []
      }

day13parser :: Parser BusSched
day13parser = do
  d <- integer
  _ <- chunk "\n"
  bs <- many busParser
  pure $
    BusSched
      { departTime = d,
        buses = filter (\(_, b) -> b /= 0) $ zip [0 ..] bs
      }

busParser :: Parser Integer
busParser = do
  x <- some digit <|> chunk "x"
  _ <- chunk "," <|> chunk "\n"
  pure $ if x == "x" then 0 else (read x :: Integer)

day13pt1 :: BusSched -> Integer
day13pt1 BusSched {departTime = d, buses = bs} = g (head ps')
  where
    cs = map snd bs
    ds = map f cs
    f x = (((d `div` x) + 1) * x) - d
    ps = zip cs ds
    ps' = sortOn snd ps
    g (a, b) = a * b

day13pt2 :: BusSched -> Integer
day13pt2 s = fst $ crt (map (first negate) (buses s))

-- Chinese remainder theorem
crt :: (Integral a, Foldable t) => t (a, a) -> (a, a)
crt = foldr go (0, 1)
  where
    go (r1, m1) (r2, m2) = (r `mod` m, m)
      where
        r = r2 + m2 * (r1 - r2) * (m2 `modinv` m1)
        m = m2 * m1

-- Modular Inverse
modinv :: Integral a => a -> a -> a
a `modinv` m = let (_, i, _) = gcde a m in i `mod` m

-- Greatest Common Divisor - Extended Euclidean Algorithm
gcde :: Integral a => a -> a -> (a, a, a)
gcde 0 b = (b, 0, 1)
gcde a b = (g, t - (b `div` a) * s, s)
  where
    (g, s, t) = gcde (b `mod` a) a