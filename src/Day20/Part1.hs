module Main where

import Flow
import Control.Lens (view)
import Day20 hiding (main)
import Day20 qualified

solve :: [Tile] -> Int
solve = cornerTiles .> map (view idn) .> product

main :: IO ()
main = Day20.main solve
