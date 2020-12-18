module Advent.Day18
  ( day18pt1,
    day18pt2,
  )
where

import Advent.ParseUtils (Parser, integer, parens, parseString, symbol)
import Control.Monad.Combinators.Expr (Operator (..), makeExprParser)
import Data.Default (Default (..))
import Text.Megaparsec ((<|>))

data Node = Val Integer | Sum Node Node | Pro Node Node deriving stock (Eq, Show)

instance Default Node where
  def = Val 0

type Table = [[Operator Parser Node]]

eval :: Node -> Integer
eval (Val x) = x
eval (Sum a b) = eval a + eval b
eval (Pro a b) = eval a * eval b

execute :: Table -> [String] -> Integer
execute t = sum . map (eval . parseString expr)
  where
    expr = makeExprParser (term expr) t
    term f = parens f <|> (Val <$> integer)

day18pt1 :: [String] -> Integer
day18pt1 = execute [[InfixL (Sum <$ symbol "+"), InfixL (Pro <$ symbol "*")]]

day18pt2 :: [String] -> Integer
day18pt2 = execute [[InfixL (Sum <$ symbol "+")], [InfixL (Pro <$ symbol "*")]]
