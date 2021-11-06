module Day15 where

import Data.List
import Data.IntMap ((!?))
import Data.IntMap.Strict qualified as IntMap

snoc :: [a] -> ([a], a)
snoc [x]    = ([], x)
snoc (x:xs) = let (xs', x') = snoc xs in (x:xs', x')

extend :: [Int] -> [Int]
extend init =
  let (init', x) = snoc init
  in init ++ go (length init) x (foldl' (\acc (i, n) -> IntMap.insert n i acc) [] (zip [1..] init'))
  where
    go i x acc = n : go (i + 1) n (IntMap.insert x i acc)
      where
        n = case acc !? x of
              Nothing -> 0
              Just x -> i - x

-- I naiively expected that the list-based implementation would fuse when
-- followed by a list lookup. This seems like a reasonable optimization, but GHC
-- doesn't currently implement it. So we have to do things the inelegant way.
extend' :: [Int] -> Int -> Int
extend' init j
  | j < length init = init !! j
  | otherwise =
    let (init', x) = snoc init
    in go (length init) j x (foldl' (\acc (i, n) -> IntMap.insert n i acc) [] (zip [1..] init'))
  where
    go i j x acc
      | i == j = n
      | otherwise = go (i + 1) j n (IntMap.insert x i acc)
      where
        n = case acc !? x of
              Nothing -> 0
              Just x -> i - x
