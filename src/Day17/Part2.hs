module Main where

import Flow

import GHC.TypeNats

import Data.Functor.Compose
import Data.Grid
import Data.Grid qualified as Grid
import Data.List qualified as List
import Data.Proxy

import Aoc2020
import Day17 qualified

step :: (KnownNat x, KnownNat y, KnownNat z, KnownNat w) => Grid [x, y, z, w] Bool -> Grid [x, y, z, w] Bool
step = autoConvolute @[3, 3, 3, 3] omitBounds $ \g ->
  let (Just x, g') = g |> getCompose |> partitionFocus
      activeNeighbors = count (== Just (Just True)) g'
  in (x && (activeNeighbors == 2 || activeNeighbors == 3)) || (not x && activeNeighbors == 3)

solve' :: (KnownNat x, KnownNat y, KnownNat z, KnownNat w) => Int -> Grid [x, y, z, w] Bool -> Int
solve' n = iterate step .> (!! n) .> count id

solve :: forall k proxy. (KnownNat k) => proxy k -> [[Bool]] -> Int
solve proxy rows =
  case (x |> fromIntegral |> someNatVal, y |> fromIntegral |> someNatVal) of
    (SomeNat (_ :: Proxy x), SomeNat (_ :: Proxy y)) ->
      expanded |> Grid.fromNestedLists' @[2*k + x, 2*k + y, 2*k + 1, 2*k + 1] |> solve' k
  where
    x = rows |> head |> length
    y = rows |> length
    k = proxy |> natVal |> fromIntegral
    wpad = False |> replicate k
    zpad = False |> replicate (2*k + 1) |> replicate k
    ypad = False |> replicate (2*k + 1) |> replicate (2*k + 1) |> replicate k
    xpad = False |> replicate (2*k + 1) |> replicate (2*k + 1) |> replicate (2*k + y) |> replicate k
    expanded
      =  rows
      |> List.transpose
      |> map (map (\z -> zpad ++ [wpad ++ [z] ++ wpad] ++ zpad))
      |> map (\y -> ypad ++ y ++ ypad)
      |> (\x -> xpad ++ x ++ xpad)

main :: IO ()
main = Day17.main solve
