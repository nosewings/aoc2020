module Main where

import Data.IntMap.Lazy (IntMap)
import Day19 hiding (main)
import Day19 qualified

fixRules :: IntMap RulePayload -> IntMap RulePayload
fixRules = id

main :: IO ()
main = Day19.main fixRules
