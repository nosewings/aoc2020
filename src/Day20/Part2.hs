module Main where

import Flow

import Control.Lens hiding ((.>), (|>))
import Control.Monad

import Data.List

import Aoc2020
import Day20 hiding (main)
import Day20 qualified

r0, r1, r2, r3, h, d, v, d' :: [[a]] -> [[a]]
r0 = id
r1 = d.h
r2 = d.h.d.h
r3 = h.d
h  = reverse
d  = transpose
v  = d.h.d
d' = h.d.h

symmetryFuncs :: [[[a]] -> [[a]]]
symmetryFuncs = [r0, r1, r2, r3, h, d, v, d']

symmetries :: [[a]] -> [[[a]]]
symmetries xs = map ($ xs) symmetryFuncs

symmetries' :: Tile -> [Tile]
symmetries' t = t |> pixels %%~ symmetries

arrange :: [Tile] -> [[Tile]]
arrange ts = makeRows (n - 1) initialRow
  where
    n = ts |> length |> fromIntegral |> sqrt |> round
    initialRow = makeRow (n - 1) initialCorner
    initialCorner = head do
      let t0 = ts |> cornerTiles |> head
      t1 <- ts
      t2 <- ts
      guard (t1 /= t0 && t2 /= t0 && t2 /= t1)
      t0' <- symmetries' t0
      t1' <- symmetries' t1
      t2' <- symmetries' t2
      guard (right t0' == left t1' && bottom t0' == top t2')
      return t0'
    makeRow 0 tile = [tile]
    makeRow m tile = tile : makeRow (m - 1) (findMatching tile right left)
    findMatching x f g = head do
      y <- ts
      guard (y /= x)
      y' <- symmetries' y
      guard (g y' == f x)
      return y'
    makeRows 0 row = [row]
    makeRows m row = row : makeRows (m - 1) (makeRow (n - 1) tile)
      where tile = findMatching (head row) bottom top

trimBorder :: [[a]] -> [[a]]
trimBorder = tail .> init .> fmap tail .> fmap init

findPattern :: (Eq a) => [[Maybe a]] -> [[a]] -> [(Int, Int)]
findPattern pat xss = do
  (i, rows) <- xss |> windows m |> zip [0..]
  (j, w) <- rows |> transpose |> windows n |> map transpose |> zip [0..]
  guard (matchRows w pat)
  return (i, j)
  where
    m = pat |> length
    n = pat |> head |> length
    matchRows rs rs' = zip rs rs' |> all (uncurry matchRow)
    matchRow  r  r'  = zip r  r'  |> all (uncurry match)
    match x = maybe True (== x)

replaceAt :: (Int, Int) -> [[Maybe a]] -> [[a]] -> [[a]]
replaceAt (i, j) pat xss = foldl' (\acc k -> acc |> ix (i + k) %~ editRow (pat !! k)) xss ([0..m-1] :: [Int])
  where
    m = pat |> length
    n = pat |> head |> length
    editRow patRow row = foldl' (\acc k -> acc |> ix (j + k) %~ edit (patRow !! k)) row ([0..n-1] :: [Int])
    edit Nothing  x = x
    edit (Just x) _ = x

fixGrid :: [Tile] -> [[Bool]]
fixGrid
  =  arrange
  .> map (map (view pixels))
  .> map (map trimBorder)
  .> map transpose
  .> map (map concat)
  .> concat

solve :: [Tile] -> Int
solve ts = head do
  f <- symmetryFuncs
  let ixs = findPattern (f searchPat) g
  guard (not . null $ ixs)
  let g' = foldl' (\acc ix -> replaceAt ix (f replacePat) acc) g ixs
  return (g' |> concat |> count id)
  where
    g = fixGrid ts
    searchPat  = map (map (\case 0 -> Nothing; 1 -> Just True )) convenience
    replacePat = map (map (\case 0 -> Nothing; 1 -> Just False)) convenience
    convenience =
      [ [0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,0]
      , [1,0,0,0,0,1,1,0,0,0,0,1,1,0,0,0,0,1,1,1]
      , [0,1,0,0,1,0,0,1,0,0,1,0,0,1,0,0,1,0,0,0]
      ]

main :: IO ()
main = Day20.main solve
