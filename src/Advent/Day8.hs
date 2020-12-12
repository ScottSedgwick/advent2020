{-# LANGUAGE DerivingStrategies #-}
module Advent.Day8
  ( day8pt1
  , day8pt2
  , day8parser
  , Term(..)
  ) where

import Advent.ParseUtils (Parser, int)
import Text.Megaparsec
import Text.Megaparsec.Char

data Action 
  = Acc Int 
  | Jmp Int 
  | Nop 
  deriving stock Show

day8parser :: Parser [Action]
day8parser = many p
  where 
    p = do
      action <- chunk "acc" <|> chunk "jmp" <|> chunk "nop"
      _      <- space
      sign   <- chunk "+" <|> chunk "-"
      value  <- int
      _      <- space
      let value' = if sign == "-" then negate value else value
      pure $ case action of
              "acc" -> Acc value'
              "jmp" -> Jmp value'
              _      -> Nop

data Term 
  = NoInstructions 
  | IndexTooLow 
  | IndexTooHigh 
  | Normal Int 
  | InfiniteLoop Int 
  deriving stock (Show, Eq)

day8pt1 :: [Action] -> Term
day8pt1 = runProgram [] 0 0 

runProgram :: [Int] -> Int -> Int -> [Action] -> Term
runProgram _ _ _ [] = NoInstructions
runProgram ds index acc xs 
  | index < 0          = IndexTooLow
  | index > length xs  = IndexTooHigh
  | index == length xs = Normal acc
  | index `elem` ds    = InfiniteLoop acc
  | otherwise          = runProgram (index:ds) index' acc' xs
    where
      (index', acc') = exec index acc xs
      exec i a ys = 
        case ys !! i of
          Acc x -> (i + 1, a + x)
          Jmp x -> (i + x, a)
          Nop   -> (i + 1, a) 

day8pt2 :: [Action] -> [Term]
day8pt2 = filter isNormal . map (runProgram [] 0 0) . flipOne 0 

isNormal :: Term -> Bool
isNormal (Normal _) = True
isNormal _ = False

flipOne :: Int -> [Action] -> [[Action]]
flipOne i xs | i >= length xs = []
             | otherwise = flipx i : flipOne (i + 1) xs
  where
    flipx j =
      case xs !! j of
        Nop     -> take j xs <> ((Jmp 0): drop (j + 1) xs)
        (Jmp _) -> take j xs <> (Nop    : drop (j + 1) xs)
        (Acc _) -> xs
