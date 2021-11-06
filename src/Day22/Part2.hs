module Main where

import Flow

import Control.Lens hiding ((.>), (|>))
import Control.Monad.State

import Data.Sequence qualified as Seq
import Data.Set (Set)
import Data.Set qualified as Set

import Aoc2020
import Day22 hiding (main)

import Day22 qualified

step :: Set GameState -> State GameState (Maybe Player)
step history = gets (Set.member ?? history) >>= \case
  True  -> return (Just One)
  False -> draw >>= \case
    (Nothing, Just y ) -> _2 =<> [y] >> return (Just Two)
    (Just x , Nothing) -> _1 =<> [x] >> return (Just One)
    (Just x , Just y ) -> do
      l1 <- gets (view _1 .> Seq.length)
      l2 <- gets (view _2 .> Seq.length)
      if | l1 >= x && l2 >= y -> do
             d1 <- gets (view _1 .> Seq.take x)
             d2 <- gets (view _2 .> Seq.take y)
             case (d1, d2) |> simulate |> fst of
               One -> _1 <>= [x, y]
               Two -> _2 <>= [y, x]
         | x < y -> _2 <>= [y, x]
         | x > y -> _1 <>= [x, y]
      return Nothing

simulate :: GameState -> (Player, GameState)
simulate = runState . go $ []
  where
    go history = do
      history' <- gets (Set.insert ?? history)
      step history >>= \case
        Nothing -> go history'
        Just x  -> return x

solve :: GameState -> Int
solve = simulate .> uncurry scoreGame

main :: IO ()
main = Day22.main solve
