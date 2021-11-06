module Main where

import Flow

import Data.List
import Data.Maybe

import Day09 hiding (main)
import Day09 qualified

lengthGe :: Int -> [a] -> Bool
lengthGe n []     = n <= 0
lengthGe n (_:xs) = lengthGe (n - 1) xs

solve :: [Int] -> Int
solve xs = minimum ns + maximum ns
  where
    ns =
      xs |>
      tails |>
      (>>= inits) |>
      filter (lengthGe 2) |>
      find (sum .> (== i)) |>
      fromJust
    i = findAnomaly xs

main :: IO ()
main = Day09.main solve
