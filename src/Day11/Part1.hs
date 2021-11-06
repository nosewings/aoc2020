module Main where

import GHC.TypeNats

import Flow

import Data.Foldable
import Data.Functor.Compose
import Data.Grid qualified as Grid

import Day11 hiding (main)
import Day11 qualified

step :: (KnownNat x, KnownNat y) => TileGrid x y -> TileGrid x y
step = Grid.autoConvolute @[3, 3] Grid.omitBounds $ \g ->
  let (Just x, g')      = g |> getCompose |> Grid.partitionFocus
      occupiedNeighbors = g' |> toList |> filter (== Just (Just Occupied)) |> length
  in case x of
    Floor    -> Floor
    Empty    -> if occupiedNeighbors == 0 then Occupied else Empty
    Occupied -> if occupiedNeighbors >= 4 then Empty else Occupied

main :: IO ()
main = Day11.main step
