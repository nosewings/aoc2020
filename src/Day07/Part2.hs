module Main where

import Flow

import Data.Foldable
import Data.Maybe
import Data.Map.Strict (Map, (!?))
import Data.Map.Strict qualified as Map

import Day07 hiding (main)
import Day07 qualified

type Graph = Map Bag (Map Bag Int)

getEdges :: Bag -> Graph -> Map Bag Int
getEdges src = Map.lookup src .> fromMaybe []

setEdges :: Bag -> Bag -> Int -> Graph -> Graph
setEdges src dst n = Map.alter (Just . maybe [(dst, n)] (Map.insert dst n)) src

compactify :: [Rule] -> Graph
compactify = foldl' insertRule []
  where
    insertRule m (Rule src edges) = foldl' (insertEdges src) m edges
    insertEdges src m (Edges n dst) = setEdges src dst n m

solve :: [Rule] -> Int
solve rules = Bag "shiny" "gold" |> solveLocally [] |> fst
  where
    g = compactify rules
    solveLocally acc src = acc !? src |> maybe (Map.foldlWithKey' continue (0, acc) (getEdges src g)) (, acc)
      where
        continue (accn, acc') dst m =
          let (n, acc'') = solveLocally acc' dst
          in (accn + m*(n + 1), acc'')

main :: IO ()
main = Day07.main solve
