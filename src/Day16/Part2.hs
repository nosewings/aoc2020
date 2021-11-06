module Main where

import Flow

import Data.Foldable
import Data.List
import Data.Map.Strict qualified as Map

import Day16 hiding (main)
import Day16 qualified

backpermute :: [a] -> [Int] -> [a]
backpermute xs = zip xs .> sortOn snd .> map fst

startsWith :: (Eq a) => [a] -> [a] -> Bool
startsWith []     _      = True
startsWith (y:ys) (x:xs) = y == x && startsWith ys xs
startsWith _      _      = False

solve :: Problem -> Int
solve Problem{..}
  =  possibleAssignments
  |> sortOn length
  |> validAssignments
  |> head
  |> backpermute (Map.keys rules)
  |> zip myTicket
  |> filter (snd .> startsWith "departure")
  |> map fst
  |> product
  where
    -- Field value columns.
    fss = nearbyTickets |> filter (not . isDefinitelyInvalidTicket rules) |> transpose

    -- List of field indices that each column could correspond to.
    possibleAssignments
      =  rules
      |> toList
      |> map \is ->
               zip [0..] fss
            |> filter (snd .> all (\f -> any (`isInInterval` f) is))
            |> map fst

    -- Convert possible assignments to valid assignments
    validAssignments [] = [[]]
    validAssignments (xs:xss) = do
      x <- xs
      let xss' = delete x <$> xss
      map (x:) $ validAssignments xss'

main :: IO ()
main = Day16.main solve
