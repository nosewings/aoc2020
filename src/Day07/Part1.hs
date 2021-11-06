module Main where

import Flow

import Data.Foldable
import Data.Maybe
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Set qualified as Set

import Day07 hiding (main)
import Day07 qualified

type Graph = Map Bag (Map Bag Int)

getEdges :: Bag -> Graph -> Map Bag Int
getEdges dst = Map.lookup dst .> fromMaybe []

setEdges :: Bag -> Bag -> Int -> Graph -> Graph
setEdges src dst n = Map.alter (Just . maybe [(src, n)] (Map.insert src n)) dst

compactify :: [Rule] -> Graph
compactify = foldl' insertRule []
  where
    insertRule m (Rule src edges) = foldl' (insertEdges src) m edges
    insertEdges src m (Edges n dst) = setEdges src dst n m

solve :: [Rule] -> Int
solve rules = Bag "shiny" "gold" |> solveLocally [] |> length
  where
    g = compactify rules
    solveLocally acc dst = Map.foldlWithKey'
      (\acc' src _ -> if src `Set.member` acc' then acc' else solveLocally (Set.insert src acc') src)
      acc
      (getEdges dst g)

main :: IO ()
main = Day07.main solve
