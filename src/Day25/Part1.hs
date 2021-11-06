module Main where

import Flow
import Aoc2020
import Day25

solve' :: Int -> Int -> (Int, Int) -> Int
solve' modulus subject (public1, public2)
  =  public1
  |> untransform modulus subject
  |> transform modulus public2

solve :: (Int, Int) -> Int
solve = solve' 20201227 7

main :: IO ()
main = mkMain'' parser solve
