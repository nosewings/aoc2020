module Main where

import Flow
import Control.Lens hiding ((.>))
import Data.Sequence (Seq)

import Day08 hiding (main)
import Day08 qualified

solve :: Seq Instr -> Int
solve = execInstrs .> view acc

main :: IO ()
main = Day08.main solve
