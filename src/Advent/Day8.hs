{-# LANGUAGE DerivingStrategies #-}

module Advent.Day8
  ( day8pt1,
    day8pt2,
    day8parser,
    Termination (..),
  )
where

import Advent.ParseUtils (Parser, int)
import Data.List (find)
import Data.Maybe (fromMaybe)
import Text.Megaparsec (chunk, many, (<|>))
import Text.Megaparsec.Char (space)

data Action
  = Acc Int
  | Jmp Int
  | Nop
  deriving stock (Show)

type Program = [Action]

day8parser :: Parser Program
day8parser = many p
  where
    p = do
      action <- chunk "acc" <|> chunk "jmp" <|> chunk "nop"
      _ <- space
      sign <- chunk "+" <|> chunk "-"
      value <- int
      _ <- space
      let value' = if sign == "-" then negate value else value
      pure $ case action of
        "acc" -> Acc value'
        "jmp" -> Jmp value'
        _ -> Nop

data Termination
  = NoInstructions
  | IndexTooLow
  | IndexTooHigh
  | Normal Int
  | InfiniteLoop Int
  | NoValidProgram
  deriving stock (Show, Eq)

day8pt1 :: Program -> Termination
day8pt1 = runProgram

runProgram :: Program -> Termination
runProgram [] = NoInstructions
runProgram xs = rp [] 0 0 xs
  where
    rp ds index acc ys
      | index < 0 = IndexTooLow
      | index > length ys = IndexTooHigh
      | index == length ys = Normal acc
      | index `elem` ds = InfiniteLoop acc
      | otherwise = rp (index : ds) index' acc' ys
      where
        (index', acc') =
          case ys !! index of
            Acc x -> (index + 1, acc + x)
            Jmp x -> (index + x, acc)
            Nop   -> (index + 1, acc)

day8pt2 :: Program -> Termination
day8pt2 = fromMaybe NoValidProgram . find isNormal . map runProgram . programPermutations 0

isNormal :: Termination -> Bool
isNormal (Normal _) = True
isNormal _ = False

programPermutations :: Int -> Program -> [Program]
programPermutations i xs
  | i >= length xs = []
  | otherwise = f : programPermutations (i + 1) xs
  where
    f =
      case xs !! i of
        Nop -> take i xs <> (Jmp 0 : drop (i + 1) xs)
        (Jmp _) -> take i xs <> (Nop : drop (i + 1) xs)
        (Acc _) -> xs
