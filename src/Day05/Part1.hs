module Main where

import Flow

import Day05 hiding (main)
import Day05 qualified

solve :: [BoardingPass] -> Int
solve = map bpId .> maximum

main :: IO ()
main = Day05.main solve
