module Main where

import GHC.TypeNats

import Control.Lens hiding (Empty, index)

import Data.Grid

import Day11 hiding (main)
import Day11 qualified

-- Dependent types seem to mess with the Flow combinators ... @$@ and @.@ might
-- be special-cased.

findOccupied :: (KnownNat x, KnownNat y) => ((Int, Int) -> (Int, Int)) -> TileGrid x y -> TileCoord x y -> Bool
findOccupied f g ix = case coord . tuple2list . f . list2tuple . unCoord $ ix of
  Nothing  -> False
  Just ix' -> case index g ix' of
    Floor    -> findOccupied f g ix'
    Empty    -> False
    Occupied -> True
  where
    list2tuple [j, i] = (j, i)
    tuple2list (j, i) = [j, i]

step :: (KnownNat x, KnownNat y) => TileGrid x y -> TileGrid x y
step g = tabulate (rule g)
  where
    rule g ix = case index g ix of
      Floor    -> Floor
      Empty    -> if numOccupied g ix == 0 then Occupied else Empty
      Occupied -> if numOccupied g ix >= 5 then Empty else Occupied
    numOccupied g ix = length . filter id $ [r g ix, l g ix, u g ix, d g ix, ru g ix, rd g ix, lu g ix, ld g ix]
    r  = findOccupied (_2 +~ 1)
    l  = findOccupied (_2 -~ 1)
    u  = findOccupied (_1 +~ 1)
    d  = findOccupied (_1 -~ 1)
    ru = findOccupied (both +~ 1)
    rd = findOccupied (\ix -> _1 -~ 1 $ _2 +~ 1 $ ix)
    lu = findOccupied (\ix -> _1 +~ 1 $ _2 -~ 1 $ ix)
    ld = findOccupied (both -~ 1)

main :: IO ()
main = Day11.main step
