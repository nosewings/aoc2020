module Main where

import Flow

import Control.Lens hiding ((.>))
import Control.Monad.Loops
import Control.Monad.State

import Aoc2020
import Day22 hiding (main)
import Day22 qualified

step :: State GameState (Maybe Player)
step = draw >>= \case
  (Nothing, Just y ) -> _2 =<> [y] >> return (Just Two)
  (Just x , Nothing) -> _1 =<> [x] >> return (Just One)
  (Just x , Just y ) -> do
    case compare x y of
      LT -> _2 <>= [y, x]
      GT -> _1 <>= [x, y]
    return Nothing

simulate :: GameState -> (Player, GameState)
simulate = runState (untilJust step)

solve :: GameState -> Int
solve = simulate .> uncurry scoreGame

main :: IO ()
main = Day22.main solve
