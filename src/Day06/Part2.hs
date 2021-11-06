module Main where

import Data.List
import Data.Set qualified as Set

import Day06 hiding (main)
import Day06 qualified

join :: [Answers] -> Answers
join = foldl1' Set.intersection

main :: IO ()
main = Day06.main join
