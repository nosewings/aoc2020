module Day03 where

import Flow
import Control.Applicative
import Text.Megaparsec.Char
import Aoc2020

data Terrain = OpenSquare | Tree
deriving instance Show Terrain
deriving instance Eq   Terrain

terrain :: Parser Terrain
terrain = (OpenSquare <$ char '.') <|> (Tree <$ char '#')

parser :: Parser [Terrain]
parser = cycle <$> many terrain

solve :: [(Int, Int)] -> [[Terrain]] -> Int
solve xys m = map (uncurry solve') xys |> product
  where
    solve' :: Int -> Int -> Int
    solve' x y =
      m |>
      iterate (drop y) |>
      takeWhile (not . null) |>
      map head |>
      map (iterate (drop x)) |>
      diagonal |>
      map head |>
      filter (== Tree) |>
      length

main :: [(Int, Int)] -> IO ()
main xys = mkMain parser (solve xys)
