module Advent.MParseUtils 
  ( Parser
  , parseFile
  , parseMap
  ) where

import Data.Void
import qualified Data.Map as M
import Text.Megaparsec

type Parser = Parsec Void String

parseFile :: Parser a -> a -> FilePath -> IO a
parseFile p d f = readFile f >>= either (\e -> print e >> pure d) pure . parse p f

parseMap :: Parser a -> Parser (M.Map (Int, Int) a)
parseMap p = some (lineParser p) >>= pure . M.fromList . concat

lineParser :: Parser a -> Parser [((Int, Int), a)]
lineParser p = some (locnParser p) >>= \xs -> eol <|> eof >> pure xs

locnParser :: Parser a -> Parser ((Int, Int), a)
locnParser p = do
  c <- p
  pos <- getSourcePos
  pure ((unPos (sourceColumn pos), unPos (sourceLine pos)), c)

eol :: Parser ()
eol = chunk "\n" >> pure ()