module Main where

import Flow

import Data.Foldable
import Data.Function
import Data.Maybe

import Day13 qualified

solve' :: Int -> [Maybe Int] -> Int
solve' t =
  catMaybes .>
  map (\x -> (x, x |> fromIntegral |> (fromIntegral t /) |> ceiling |> fromIntegral |> (* x) |> subtract t)) .>
  minimumBy (compare `on` snd) .>
  uncurry (*)

main :: IO ()
main = Day13.main solve'
