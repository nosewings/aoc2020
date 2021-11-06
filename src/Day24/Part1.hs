module Main where

import Flow

import Day24 hiding (main)
import Day24 qualified

solve :: [[Direction]] -> Int
solve = initialize .> length

main :: IO ()
main = Day24.main solve
