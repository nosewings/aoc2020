module Main where

import Flow
import Aoc2020
import Day10 qualified

solve :: Int -> [Int] -> Int -> Int
solve a xs z = count (== 1) diffs * count (== 3) diffs
  where
    diffs =
      xs |>
      (0:) |>
      (++ [z]) |>
      windows2 |>
      map (uncurry (flip (-)))

main :: IO ()
main = Day10.main solve
