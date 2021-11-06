module Main where

import Day02 (Password(..))
import Day02 qualified

isValid :: Password -> Bool
isValid (Password l u c s) = (s !! l' == c) /= (s !! u' == c)
  where
    l' = l - 1
    u' = u - 1

main :: IO ()
main = Day02.main isValid
