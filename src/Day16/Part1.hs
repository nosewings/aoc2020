module Main where

import Flow

import Day16 hiding (main)
import Day16 qualified

solve :: Problem -> Int
solve Problem{..} = nearbyTickets |> concat |> filter (isDefinitelyInvalidField rules) |> sum

main :: IO ()
main = Day16.main solve
