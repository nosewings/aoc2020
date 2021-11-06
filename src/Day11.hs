module Day11 where

import GHC.TypeNats

import Flow

import Data.Grid (Coord, Grid)
import Data.Grid qualified as Grid
import Data.Foldable
import Data.Maybe
import Data.Proxy

import Text.Megaparsec
import Text.Megaparsec.Char

import Aoc2020

data Tile = Floor | Empty | Occupied
deriving instance (Show Tile)
deriving instance (Eq   Tile)

parser :: Parser [Tile]
parser = many (Floor <$ char '.' <|> Empty <$ char 'L')

type TileGrid x y = Grid [y, x] Tile
type TileCoord x y = Coord [y, x]
type StepFunction = forall x y. (KnownNat x, KnownNat y) => TileGrid x y -> TileGrid x y

solve' :: (KnownNat x, KnownNat y) => StepFunction -> TileGrid x y -> Int
solve' step = findFixpoint step .> fromJust .> toList .> filter (== Occupied) .> length

solve :: StepFunction -> [[Tile]] -> Int
solve step tss =
  case (tss |> length |> fromIntegral |> someNatVal, tss |> head |> length |> fromIntegral |> someNatVal) of
    (SomeNat (_ :: Proxy y), SomeNat (_ :: Proxy x)) ->
      tss |> Grid.fromNestedLists' @[y, x] |> solve' step

main :: StepFunction -> IO ()
main step = mkMain parser (solve step)
