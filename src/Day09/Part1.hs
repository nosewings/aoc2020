module Main where

import Day09 hiding (main)
import Day09 qualified

solve :: [Int] -> Int
solve = findAnomaly

main :: IO ()
main = Day09.main solve
