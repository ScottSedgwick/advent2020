module Main (main) where

import Advent
import Text.Parsec
import Text.Parsec.String ( Parser )

main :: IO ()
-- main = interact (interact' ints day10pt1)
main = interact (interact' ints day10pt2)

interact' :: Parser [Int] -> ([Int] -> Int) -> String -> String
interact' p f s =
  case parse p "" s of
    (Left e)  -> "Parse error: " <> show e
    (Right a) -> show (f a) 
  <> "\n"
