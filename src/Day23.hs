module Day23 where

import Flow

main :: ([Int] -> Int) -> IO ()
main solve = interact $ map ((:[]) .> read) .> solve .> show .> (++ "\n")
