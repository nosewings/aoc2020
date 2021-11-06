module Day10 where

import Data.List
import Text.Megaparsec.Char.Lexer
import Aoc2020

parser :: Parser Int
parser = decimal

main :: (Int -> [Int] -> Int -> Int) -> IO ()
main solve = mkMain parser (\xs -> solve 0 (sort xs) (maximum xs + 3))
