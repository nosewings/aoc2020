module Main where

import Flow

import Data.Maybe
import Data.List
import Data.Tuple

import Aoc2020
import Day13 qualified

-- Extended Euclidean algorithm.
euclid :: Int -> Int -> (Int, Int, Int)
euclid a b = terms |> windows2 |> find done |> fromJust |> fst
  where
    terms = (a, 1, 0) : (b, 0, 1) : zipWith next terms (tail terms)
    next (r0, s0, t0) (r1, s1, t1) =
      let (q, r2) = r0 `divMod` r1
      in (r2, s0 - q*s1, t0 - q*t1)
    done (_, (r1, _, _)) = r1 == 0

-- Chinese remainder theorem.
crt :: [(Int, Int)] -> Int
crt pairs = zipWith3 (\a p b -> a*p*b) as ps bs |> sum |> (`mod` n) where
  ns = fst <$> pairs
  n = product ns
  as = snd <$> pairs
  ps = map (product ns `div`) ns
  bs = zipWith euclid ps ns |> map (\(_, b, _) -> b)

solve' :: Int -> [Maybe Int] -> Int
solve' _ = zipWith (fmap . (,)) [0, -1..] .> catMaybes .> map swap .> crt

main :: IO ()
main = Day13.main solve'
