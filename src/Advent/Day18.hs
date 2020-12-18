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

expr1 :: Parser Node
expr1 = makeExprParser (parens expr1 <|> (Val <$> integer)) [[InfixL (Sum <$ symbol "+"), InfixL (Pro <$ symbol "*")]]

expr2 :: Parser Node
expr2 = makeExprParser (parens expr2 <|> (Val <$> integer)) [[InfixL (Sum <$ symbol "+")], [InfixL (Pro <$ symbol "*")]]

eval :: Node -> Integer
eval (Val x) = x
eval (Sum a b) = eval a + eval b
eval (Pro a b) = eval a * eval b

execute :: Parser Node -> [String] -> Integer
execute f = sum . map (eval . parseString f)

day18pt1 :: [String] -> Integer
day18pt1 = execute expr1

day18pt2 :: [String] -> Integer
day18pt2 = execute expr2
