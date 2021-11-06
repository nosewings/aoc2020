module Main where

import Data.List
import Data.Map.Strict qualified as Map

import Day04 hiding (main)
import Day04 qualified

isValid :: Passport -> Bool
isValid p = sort ["byr", "iyr", "eyr", "hgt", "hcl", "ecl", "pid"] `isSubsequenceOf` Map.keys p

main :: IO ()
main = Day04.main isValid
