module Main where

import Flow

import Control.Monad.Reader

import Data.Foldable
import Data.List
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Set (Set)
import Data.Set qualified as Set

import Day21 hiding (main)
import Day21 qualified

validAssignments :: Map String (Set String) -> [Map String String]
validAssignments [] = [[]]
validAssignments m  = do
  let ([(allergen, ingredients)], m') = Map.splitAt 1 m
  ingredient <- toList ingredients
  let m'' = Set.delete ingredient <$> m'
  rest <- validAssignments m''
  return $ [(allergen, ingredient)] <> rest

solve :: [Food] -> String
solve
  =  runReader possibleAssignments
  .> validAssignments
  .> head
  .> Map.toList
  .> sortOn fst
  .> map snd
  .> intercalate ","

main :: IO ()
main = Day21.main solve
