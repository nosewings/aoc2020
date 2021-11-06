module Day20 where

import Flow
import Control.Lens hiding ((.>), (|>), (<.))
import Control.Monad.State
import Data.Set qualified as Set

import Text.Megaparsec hiding (State, count, match)
import Text.Megaparsec.Char
import Text.Megaparsec.Char.Lexer

import Aoc2020

data Tile = Tile
  { _idn    :: Int
  , _pixels :: [[Bool]]
  }
makeLenses ''Tile
deriving instance Show Tile

instance Eq Tile where
  t1 == t2 = t1^.idn == t2^.idn

parser :: Parser Tile
parser = tile
  where
    tile = do
      _    <- "Tile "
      n    <- decimal
      _    <- char ':'
      _    <- eol
      rows <- row `endBy` eol
      pure $ Tile n rows
    row = do
      cs <- takeWhile1P Nothing (\c -> c == '.' || c == '#')
      pure $ map (\case '.' -> False; '#' -> True) cs

top, right, bottom, left :: Tile -> [Bool]
top    = view pixels .> head
right  = view pixels .> map last
bottom = view pixels .> last
left   = view pixels .> map head

borders :: Tile -> [[Bool]]
borders x = map ($ x) [top, right, bottom, left]

cornerTiles :: [Tile] -> [Tile]
cornerTiles xs = filter isCorner xs
  where
    isCorner y = length (matchingBorders y) == 2
    matchingBorders y = Set.fromList do
      x <- xs
      guard (x /= y)
      by <- borders y
      bx <- borders x
      guard (bx == by || reverse bx == by)
      [by]

main :: ([Tile] -> Int) -> IO ()
main = mkMain parser
