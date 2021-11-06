module Day13 where

import Text.Megaparsec
import Text.Megaparsec.Char
import Text.Megaparsec.Char.Lexer

import Aoc2020

parser :: Parser (Int, [Maybe Int])
parser = do
  n   <- decimal
  _   <- eol
  mns <- entry `sepBy` char ','
  _   <- eol
  pure (n, mns)
  where
    entry = Just <$> decimal <|> Nothing <$ char 'x'

main :: (Int -> [Maybe Int] -> Int) -> IO ()
main solve = mkMain'' parser (uncurry solve)
