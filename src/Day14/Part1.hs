module Main where

import Flow

import Control.Lens hiding ((.>), (|>))

import Data.Map.Strict qualified as Map
import Data.Maybe

import Day14 hiding (main)
import Day14 qualified

applyBitmask :: Bitmask -> Number -> Number
applyBitmask = zipWith (flip fromMaybe)

step :: Statement -> State -> State
step (Mask bm) s = s |> mask .~ bm
step (Mem i n) s = s |> mem  %~ Map.insert i (applyBitmask (s^.mask) n)

main :: IO ()
main = Day14.main step
