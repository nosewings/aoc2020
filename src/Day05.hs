module Day05 where

import Control.Monad

import Data.Foldable

import Text.Megaparsec
import Text.Megaparsec.Char

import Aoc2020

data BspIx = L | U
deriving instance Show BspIx

bsp :: BspIx -> (Int, Int) -> (Int, Int)
bsp L (l, u) = (l, l + ((u - l) `div` 2))
bsp U (l, u) = (l + ((u - l) `div` 2), u)

type BoardingPass = ([BspIx], [BspIx])

index :: BoardingPass -> (Int, Int)
index (row, col) =
  (fst (foldl' (flip bsp) (0, 128) row), fst (foldl' (flip bsp) (0, 8) col))

bpId :: BoardingPass -> Int
bpId bp = 8 * row + col
  where (row, col) = index bp

parser :: Parser BoardingPass
parser = do
  row <- replicateM 7 rowChar
  col <- replicateM 7 colChar
  pure (row, col)
  where
    rowChar = (L <$ char 'F') <|> (U <$ char 'B')
    colChar = (L <$ char 'L') <|> (U <$ char 'R')

main :: ([BoardingPass] -> Int) -> IO ()
main = mkMain parser
