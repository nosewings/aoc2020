module Day24 where

import Flow

import Data.Foldable
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Monoid.Odd
import Data.MultiSet (MultiSet)
import Data.MultiSet qualified as MultiSet
import Data.Set (Set)

import Text.Megaparsec

import Aoc2020

data Direction = E | SE | SW | W | NW | NE
deriving instance Show    Direction
deriving instance Eq      Direction
deriving instance Ord     Direction
deriving instance Enum    Direction
deriving instance Bounded Direction

parser :: Parser [Direction]
parser = many (e <|> se <|> sw <|> w <|> nw <|> ne)
  where
    e  = E  <$ "e"
    se = SE <$ "se"
    sw = SW <$ "sw"
    w  = W  <$ "w"
    nw = NW <$ "nw"
    ne = NE <$ "ne"

newtype Translation = Translation (MultiSet Direction)
deriving instance Show Translation
deriving instance Eq   Translation
deriving instance Ord  Translation

mkTranslation :: MultiSet Direction -> Translation
mkTranslation = expand .> cancel .> Translation
  where
    expand = expandTo E NE SE .> expandTo W NW SW
    cancel = cancelFrom NE SW .> cancelFrom NW SE
    expandTo dx dy dz m = m |> MultiSet.deleteAll dx |> MultiSet.insertMany dy x |> MultiSet.insertMany dz x
      where x = MultiSet.occur dx m
    cancelFrom dx dy m = m |> MultiSet.deleteMany dx (MultiSet.occur dy m) |> MultiSet.deleteMany dy (MultiSet.occur dx m)

instance Semigroup Translation where
  Translation m1 <> Translation m2 = mkTranslation (m1 <> m2)

instance Monoid Translation where
  mempty = Translation MultiSet.empty

lower :: [Direction] -> Translation
lower = MultiSet.fromList .> mkTranslation

directions :: [Translation]
directions = ((:[]) .> lower) <$> [minBound..maxBound]

neighbors :: Translation -> [Translation]
neighbors t = map (t <>) directions

newtype Func k v = Func { getFunc :: Map k v }
deriving instance (Show k, Show v) => Show (Func k v)

instance (Ord k, Semigroup v) => Semigroup (Func k v) where
  Func m1 <> Func m2 = Func $ Map.unionWith (<>) m1 m2

instance (Ord k, Monoid v) => Monoid (Func k v) where
  mempty = Func []

initialize :: [[Direction]] -> Set Translation
initialize = foldMap' (\k -> Func [(lower k, Odd True)]) .> getFunc .> Map.filter (== Odd True) .> Map.keysSet

main :: ([[Direction]] -> Int) -> IO ()
main = mkMain parser
