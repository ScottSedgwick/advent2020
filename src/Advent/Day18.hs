module Advent.Day18
  ( day18pt1
  , day18pt2
  ) where
  
import Control.Monad.Combinators.Expr ( makeExprParser, Operator(..) )
import Data.Default ( Default(..) )
import Text.Megaparsec ( (<|>), (<?>) )
import Advent.ParseUtils ( integer, Parser, symbol, parens, parseString )

data Node
  = Val Integer
  | Sum Node Node
  | Pro Node Node
  deriving stock (Eq, Show)

instance Default Node where
  def = Val 0

expr1 :: Parser Node
expr1 = makeExprParser term1 table1
  where
    table1 = [ [ InfixL (Pro <$ symbol "*")
               , InfixL (Sum <$ symbol "+") 
               ] 
             ]
    term1  = parens expr1 <|> (Val <$> integer) <?> "term"

expr2 :: Parser Node
expr2 = makeExprParser term2 table2
  where
    table2 = [ [ InfixL (Sum <$ symbol "+") ]
             , [ InfixL (Pro <$ symbol "*") ]
             ]
    term2  = parens expr2 <|> (Val <$> integer) <?> "term"

eval :: Node -> Integer
eval (Val x)   = x
eval (Sum a b) = eval a + eval b
eval (Pro a b) = eval a * eval b

day18pt1 :: [String] -> Integer
day18pt1 = sum . map (eval . parseString expr1)

day18pt2 :: [String] -> Integer
day18pt2 = sum . map (eval . parseString expr2)
