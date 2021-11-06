module Day09 where

import Flow

import Data.List
import Data.Maybe

import Text.Megaparsec.Char.Lexer

import Aoc2020

parser :: Parser Int
parser = decimal

findAnomaly :: [Int] -> Int
findAnomaly xs = xs |> drop 25 |> flip zip sums |> find (uncurry notElem) |> fromJust |> fst
  where
    wins = xs |> windows 25
    sums = [[x + x' | x <- xs, x' <- xs] | xs <- wins]

main :: ([Int] -> Int) -> IO ()
main = mkMain parser
