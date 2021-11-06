module Main where

import Control.Monad.Combinators.Expr

import Text.Megaparsec
import Text.Megaparsec.Char
import Text.Megaparsec.Char.Lexer (decimal)
import Text.Megaparsec.Char.Lexer qualified

import Aoc2020
import Day18 hiding (main)
import Day18 qualified

parser :: Parser Exp
parser = exp where
  exp = makeExprParser term table
  term = parens exp <|> num
  parens = between (symbol "(") (symbol ")")
  num = Num <$> lexeme decimal
  table = [[binary Add "+"], [binary Mul "*"]]
  binary f s = InfixL (f <$ symbol s)
  space = Text.Megaparsec.Char.Lexer.space hspace1 empty empty
  lexeme = Text.Megaparsec.Char.Lexer.lexeme space
  symbol = Text.Megaparsec.Char.Lexer.symbol space

main :: IO ()
main = Day18.main parser
