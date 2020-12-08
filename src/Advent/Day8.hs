{-# LANGUAGE DerivingStrategies #-}
module Advent.Day8
  ( day8pt1
  , day8pt2
  ) where

day8pt1 :: String -> Term
day8pt1 = runProgram [] 0 0 . parseAll

data Action 
  = Acc Int 
  | Jmp Int 
  | Nop 
  deriving stock Show

data Term 
  = NoInstructions 
  | IndexTooLow 
  | IndexTooHigh 
  | Normal Int 
  | InfiniteLoop Int 
  deriving stock Show

parse :: String -> Action
parse ('a':'c':'c':' ':'+':xs) = Acc (read xs :: Int)
parse ('a':'c':'c':' ':'-':xs) = Acc (-1 * read xs :: Int)
parse ('j':'m':'p':' ':'+':xs) = Jmp (read xs :: Int)
parse ('j':'m':'p':' ':'-':xs) = Jmp (-1 * read xs :: Int)
parse _ = Nop

parseAll :: String -> [Action]
parseAll = map parse . lines 

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

day8pt2 :: String -> [Term]
day8pt2 = filter isNormal . map (runProgram [] 0 0) . flipOne 0 . map parse . lines

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
