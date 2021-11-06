module Day25 where

import Flow

import Data.Bits
import Data.IntMap.Strict ((!?))
import Data.IntMap.Strict qualified as IntMap
import Data.List
import Data.Maybe

import Text.Megaparsec.Char
import Text.Megaparsec.Char.Lexer

import Aoc2020

parser :: Parser (Int, Int)
parser = do
  m <- decimal
  _ <- eol
  n <- decimal
  _ <- eol
  pure (m, n)

-- Naiive modular exponentiation (nonnegative exponent).
expMod' :: Int -> Int -> Int -> Int
expMod' a e (abs -> n)
  | e < 0 = error "expMod': negative exponent"
  | otherwise
    =  1
    |> (`mod` n)
    |> iterate (\b -> b * a `mod` n)
    |> (!! e)

transform' :: Int -> Int -> Int -> Int
transform' modulus subject private = expMod' subject private modulus

-- Naiive discrete logarithm.
discreteLog' :: Int -> Int -> Int -> Maybe Int
discreteLog' a' b' n'
  | n == 0       = error "discreteLog': zero modulus"
  | gcd a n /= 1 = error "discreteLog': base not coprime to modulus"
  | otherwise    = go (1 `mod` n) 0
  where
    a = a' `mod` n
    b = b' `mod` n
    n = abs n'
    go x i
      | x == b    = Just i
      | i == n    = Nothing
      | otherwise = go (x * a `mod` n) $! i + 1

untransform' :: Int -> Int -> Int -> Int
untransform' modulus subject public = discreteLog' subject public modulus |> fromJust

-- Fast modular exponentiation (nonnegative exponent) via repeated squaring.
expMod :: Int -> Int -> Int -> Int
expMod a e (abs -> n)
  | e < 0 = error "expMod: negative exponent"
  | otherwise
    =  a
    |> iterate (\x -> (x * x) `rem` n)
    |> zip [0 .. finiteBitSize a - 1]
    |> filter (fst .> testBit e)
    |> map snd
    |> foldl' (\acc x -> acc * x `rem` n) 1
    |> (`mod` n)

transform :: Int -> Int -> Int -> Int
transform modulus subject private = expMod subject private modulus

-- Fast discrete logarithm using baby-step giant-step.
-- Assumes that the modulus is prime.
discreteLog :: Int -> Int -> Int -> Maybe Int
discreteLog a' b' n'
  | n == 0 = error "discreteLog': zero modulus"
  | otherwise
    =  b
    |> iterate (\e -> e * factor `mod` n)
    |> zip [0 .. m - 1]
    |> map (\(i, e) -> (i,) <$> table !? e)
    |> catMaybes
    |> listToMaybe
    |> fmap (\(i, j) -> i * m + j)
  where
    a = a' `mod` n
    b = b' `mod` n
    n = abs n'
    m = n |> fromIntegral |> sqrt |> ceiling
    table = 1 |> iterate (\e -> e * a `mod` n) |> (`zip` [0 .. m - 1]) |> IntMap.fromList
    factor = expMod a (m * (n - 2)) n

untransform :: Int -> Int -> Int -> Int
untransform modulus subject public = discreteLog subject public modulus |> fromJust
