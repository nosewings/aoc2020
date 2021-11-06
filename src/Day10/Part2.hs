module Main where

import Flow

import Control.Monad.State

import Data.List
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Maybe

import Day10 qualified

solve :: Int -> [Int] -> Int -> Int
solve a xs z = evalState (go a xs) []
  where
    go :: Int -> [Int] -> State (Map (Int, [Int]) Int) Int
    go a xs = gets (Map.lookup (a, xs)) >>= \case
      Just ret -> return ret
      Nothing  -> do
        ret <- if null xs then return (if z <= a + 3 then 1 else 0) else do
          let ps = xs |> tails |> map uncons |> catMaybes |> takeWhile (fst .> (<= a + 3))
          sum <$> traverse (uncurry go) ps
        modify (Map.insert (a, xs) ret)
        return ret

main :: IO ()
main = Day10.main solve
