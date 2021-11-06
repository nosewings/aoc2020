module Main where

import Data.Set qualified as Set

import Day06 hiding (main)
import Day06 qualified

join :: [Answers] -> Answers
join = Set.unions

main :: IO ()
main = Day06.main join
