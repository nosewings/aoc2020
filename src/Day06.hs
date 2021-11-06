module Day06 where

import Flow

import Data.Char
import Data.Set (Set)
import qualified Data.Set as Set

import Text.Megaparsec
import Text.Megaparsec.Char

import Aoc2020

type Answers = Set Char

parser :: Parser [Answers]
parser = map Set.fromList <$> line `endBy` char '\n'
  where
    line = takeWhile1P Nothing (not . isSpace)

solve :: ([Answers] -> Answers) -> [[Answers]] -> Int
solve join = map (join .> length) .> sum

main :: ([Answers] -> Answers) -> IO ()
main join = mkMain' parser (solve join)
