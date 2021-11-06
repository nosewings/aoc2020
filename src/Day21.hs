module Day21 where

import Flow

import Control.Lens hiding ((.>), (|>))
import Control.Monad.Reader

import Data.Char
import Data.Foldable
import Data.List
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Set (Set)
import Data.Set qualified as Set

import Text.Megaparsec
import Text.Megaparsec.Char

import Aoc2020

data Food = Food
  { ingredients :: [String]
  , allergens   :: [String]
  }
deriving instance Show Food

allIngredients :: Reader [Food] (Set String)
allIngredients = asks (concatMap ingredients .> Set.fromList)

allAllergens :: Reader [Food] (Set String)
allAllergens = asks (concatMap allergens .> Set.fromList)

foodsContainingAllergen :: String -> Reader [Food] [Food]
foodsContainingAllergen a = asks $ filter (allergens .> (a `elem`))

parser :: Parser Food
parser = Food <$> ingredients <*> allergens
  where
    ingredients = word `endBy` char ' '
    allergens = do
      _   <- "(contains "
      ret <- word `sepBy` ", "
      _   <- char ')'
      pure ret
    word = takeWhile1P Nothing isAsciiLower

possibleAssignments :: Reader [Food] (Map String (Set String))
possibleAssignments = allAllergens >>= foldlM (\acc x -> (Map.insert x ?? acc) <$> step x) []
  where
    step x = foldl1' Set.intersection . map (Set.fromList . ingredients) <$> foodsContainingAllergen x

main :: ([Food] -> String) -> IO ()
main = mkMainStr parser
