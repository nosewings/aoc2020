module Main where

import Flow

import Data.IntMap (IntMap)
import Data.IntMap qualified as IntMap

import Day19 hiding (main)
import Day19 qualified

fixRules :: IntMap RulePayload -> IntMap RulePayload
fixRules
  =  IntMap.insert 8 (Nonterminal [[42], [42, 8]])
  .> IntMap.insert 11 (Nonterminal [[42, 31], [42, 11, 31]])

main :: IO ()
main = Day19.main fixRules
