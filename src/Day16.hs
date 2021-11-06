module Day16 where

import Flow

import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Monoid

import Text.Megaparsec
import Text.Megaparsec.Char
import Text.Megaparsec.Char.Lexer

import Aoc2020

data Interval = Interval Int Int
deriving instance Show Interval

isInInterval :: Interval -> Int -> Bool
isInInterval (Interval l u) n = l <= n && n <= u

type Rules = Map String [Interval]

isDefinitelyInvalidField :: Rules -> Int -> Bool
isDefinitelyInvalidField rs n = rs |> foldMap (Any . any (`isInInterval` n)) |> getAny |> not

type Ticket = [Int]

isDefinitelyInvalidTicket :: Rules -> Ticket -> Bool
isDefinitelyInvalidTicket rs = any (isDefinitelyInvalidField rs)

data Problem = Problem
  { rules         :: Rules
  , myTicket      :: Ticket
  , nearbyTickets :: [Ticket]
  }
deriving instance Show Problem

parser :: Parser Problem
parser = do
  rs <- try rule `endBy` eol
  _  <- eol
  t  <- myTicket
  _  <- eol
  ts <- nearbyTickets
  pure $ Problem (Map.fromList rs) t ts
  where

    rule = do
      k <- takeWhile1P Nothing (/= ':')
      _ <- ": "
      v <- interval `sepBy` " or "
      pure (k, v)
    interval = do
      l <- decimal
      _ <- char '-'
      u <- decimal
      pure $ Interval l u

    myTicket = do
      _ <- "your ticket:"
      _ <- eol
      t <- ticket
      _ <- eol
      pure t
    nearbyTickets = do
      _ <- "nearby tickets:"
      _ <- eol
      ticket `sepBy` eol
    ticket = decimal `sepBy` char ','

main :: (Problem -> Int) -> IO ()
main = mkMain'' parser
