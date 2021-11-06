module Day07 where

import Data.Char

import Text.Megaparsec
import Text.Megaparsec.Char
import Text.Megaparsec.Char.Lexer

import Aoc2020

data Rule = Rule Bag [Edges]
deriving instance Show Rule

data Bag = Bag String String
deriving instance Show Bag
deriving instance Eq   Bag
deriving instance Ord  Bag

data Edges = Edges Int Bag
deriving instance Show Edges

parser :: Parser Rule
parser = rule
  where
    rule = do
      src  <- bag
      _    <- kwContain
      dsts <- edgess
      _    <- dot
      pure $ Rule src dsts
    bag = do
      adj <- word
      col <- word
      _   <- kwBags
      pure $ Bag adj col
    edgess = ([] <$ "no other bags") <|> (edges `sepBy` comma)
    edges = Edges <$> number <*> bag

    kwContain = do
      _ <- "contain"
      _ <- hspace
      pure ()
    kwBags = do
      _ <- "bag"
      _ <- optional (char 's')
      _ <- hspace
      pure ()
    word = takeWhile1P Nothing isAlpha <* hspace
    number = decimal <* hspace
    comma = char ',' <* hspace
    dot = char '.'

main :: ([Rule] -> Int) -> IO ()
main = mkMain parser
