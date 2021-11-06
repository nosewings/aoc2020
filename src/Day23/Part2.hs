module Main where

import Flow

import Control.Monad.ST

import Data.List
import Data.Maybe

import Data.Vector.Unboxed (Vector, (!))
import Data.Vector.Unboxed qualified as Vector
import Data.Vector.Unboxed.Mutable (MVector)
import Data.Vector.Unboxed.Mutable qualified as MVector

import Aoc2020
import Day23 qualified

create :: [Int] -> ST s (MVector s Int)
create []     = MVector.new 0
create (x:xs) = do
  acc <- MVector.unsafeNew (length (x:xs))
  initialize (x:xs) acc
  return acc
  where
    initialize [y]       acc = MVector.write acc (y - 1) x
    initialize (y:y':ys) acc = do
      MVector.write acc (y - 1) y'
      initialize (y':ys) acc

step :: Int -> MVector s Int -> ST s Int
step x xs = do
  [_, a, b, c, y] <- iterateM (\i -> MVector.read xs (i - 1)) 5 x
  MVector.write xs (x - 1) y
  let n = MVector.length xs
      u = [x - 1, x - 2 .. 1] ++ [n, n - 1 ..] |> find (\u -> u /= a && u /= b && u /= c) |> fromJust
  v <- MVector.read xs (u - 1)
  MVector.write xs (u - 1) a
  MVector.write xs (c - 1) v
  return y

readout :: Vector Int -> [Int]
readout v = 1 : go 1
  where
    go x =
      let y = v ! (x - 1)
      in if y == 1 then [] else y : go y

solve' :: Int -> [Int] -> Vector Int
solve' n xs = Vector.create do
  v <- create xs
  go n v (head xs)
  return v
  where
    go 0 v _ = return v
    go n v x = step x v >>= go (n - 1) v

solve :: [Int] -> Int
solve xs = i * j
  where
    result = solve' 10000000 (xs ++ [maximum xs + 1 .. 1000000])
    i = result ! 0
    j = result ! (i - 1)

main :: IO ()
main = Day23.main solve
