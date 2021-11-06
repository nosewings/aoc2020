module Main where

import Flow

import Data.Maybe

import Data.Sequence (Seq(..))
import Data.Sequence qualified as Seq

import Day23 qualified

step :: Seq Int -> Seq Int
step s@(x:<|xs) = xs' :|> x
  where
    xs' = pfx <> hd <> sfx
    (pfx, sfx) = Seq.splitAt (ix + 1) tl
    ix = tl |> Seq.findIndexL (== label) |> fromJust
    label = [x - 1, x - 2 .. 1] ++ [Seq.length s, Seq.length s - 1 ..] |> dropWhile (`elem` hd) |> head
    (hd, tl) = Seq.splitAt 3 xs

finalize :: Seq Int -> Seq Int
finalize xs =
  let (hd, tl) = Seq.breakl (== 1) xs
  in Seq.drop 1 tl <> hd

solve :: [Int] -> Int
solve = Seq.fromList .> iterate step .> (!! 100) .> finalize .> foldMap show .> read

main :: IO ()
main = Day23.main solve
