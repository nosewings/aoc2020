module Day22 where

import Flow

import Control.Lens hiding ((.>))
import Control.Monad.Loops
import Control.Monad.State

import Data.Sequence (Seq)
import Data.Sequence qualified as Seq

import Text.Megaparsec hiding (State)
import Text.Megaparsec.Char
import Text.Megaparsec.Char.Lexer

import Aoc2020

type Card = Int
type Deck = Seq Card

type GameState = (Deck, Deck)

data Player = One | Two
deriving instance Show Player

project :: Player -> ((a, a) -> a)
project One = fst
project Two = snd

parser :: Parser GameState
parser = do
  _  <- "Player 1:"
  _  <- eol
  d1 <- decimal `endBy` eol
  _  <- eol
  _  <- "Player 2:"
  _  <- eol
  d2 <- decimal `endBy` eol
  pure (Seq.fromList d1, Seq.fromList d2)

draw :: State GameState (Maybe Card, Maybe Card)
draw = do
  x <- zoom _1 $ pullMaybeS uncons
  y <- zoom _2 $ pullMaybeS uncons
  pure (x, y)

scoreDeck :: Deck -> Card
scoreDeck = Seq.reverse .> Seq.foldlWithIndex (\acc i x -> acc + (i + 1) * x) 0

scoreGame :: Player -> GameState -> Card
scoreGame p = project p .> scoreDeck

main :: (GameState -> Card) -> IO ()
main = mkMain'' parser
