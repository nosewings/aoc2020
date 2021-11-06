module Main where

import Flow

import Control.Lens hiding ((.>), (|>))

import Data.Foldable
import Data.Map.Strict qualified as Map

import Day14 hiding (main)
import Day14 qualified

decodeBitmask :: Bitmask -> Number -> [Number]
decodeBitmask ms bs = zip ms bs |> foldr (\(m, b) bs -> f m b >>= \b -> fmap (b:) bs) [[]]
  where
    f :: Maybe Bool -> Bool -> [Bool]
    f Nothing      = const [False, True]
    f (Just False) = (:[])
    f (Just True)  = const [True]

step :: Statement -> State -> State
step (Mask bm) s = s |> mask .~ bm
step (Mem i n) s = s |> mem  %~ \m -> foldl' (\m i -> Map.insert i n m) m (decodeBitmask (s^.mask) i)

main :: IO ()
main = Day14.main step
