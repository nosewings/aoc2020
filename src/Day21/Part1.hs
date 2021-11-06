module Main where

import Flow

import Control.Monad.Reader

import Data.Foldable
import Data.Set (Set)
import Data.Set qualified as Set

import Aoc2020
import Day21 hiding (main)
import Day21 qualified

possibleAllergenicIngredients :: Reader [Food] (Set String)
possibleAllergenicIngredients = fold <$> possibleAssignments

solve' :: Reader [Food] Int
solve' = foldM countOf 0 =<< Set.difference <$> allIngredients <*> possibleAllergenicIngredients
  where
    countOf :: Int -> String -> Reader [Food] Int
    countOf n x = asks (map ingredients .> count (x `elem`) .> (+ n))

solve :: [Food] -> String
solve = runReader solve' .> show

main :: IO ()
main = Day21.main solve
