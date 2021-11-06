module Main where

import Flow

import Data.Foldable
import Data.MultiSet qualified as MultiSet
import Data.Set (Set)
import Data.Set qualified as Set

import Day24 hiding (main)
import Day24 qualified

step :: Set Translation -> Set Translation
step blks = blks |> Set.union blkNeighbors |> foldMap' rule
  where
    neighborCounts = blks |> foldMap' (neighbors .> MultiSet.fromList)
    blkNeighbors = neighborCounts |> MultiSet.toSet
    rule :: Translation -> Set Translation
    rule t = Set.fromList [t | if t `elem` blks then not (neighbors == 0 || neighbors > 2) else neighbors == 2]
      where neighbors = MultiSet.occur t neighborCounts

solve' :: Int -> [[Direction]] -> Int
solve' n = initialize .> iterate step .> (!! n) .> length

solve :: [[Direction]] -> Int
solve = solve' 100

main :: IO ()
main = Day24.main solve
