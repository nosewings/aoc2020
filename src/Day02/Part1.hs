module Main where

import Flow

import Day02 (Password(..))
import Day02 qualified

isValid :: Password -> Bool
isValid (Password l u c s) = l <= n && n <= u
  where n = s |> filter (== c) |> length

main :: IO ()
main = Day02.main isValid
