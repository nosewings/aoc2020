module Main where

import Flow

import Data.List
import Data.Maybe

import Day05 hiding (main)
import Day05 qualified

import Aoc2020

solve :: [BoardingPass] -> Int
solve =
  map bpId .>
  sort .>
  windows2 .>
  find (\(x, y) -> x + 1 /= y) .>
  fromJust .>
  fst .>
  (+ 1)

main :: IO ()
main = Day05.main solve
