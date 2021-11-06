module Day01 where

import Flow

import Text.Megaparsec.Char.Lexer

import Aoc2020

parser :: Parser Int
parser = decimal

solve :: Int -> [Int] -> Int
solve n xs =
  xs |>
  replicate n |>
  sequence |>
  filter (sum .> (== 2020)) |>
  head |>
  product

main :: Int -> IO ()
main n = mkMain parser (solve n)
