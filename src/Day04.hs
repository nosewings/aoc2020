module Day04 where

import Flow

import Data.Char
import Data.Map.Strict qualified as Map
import Data.Map.Strict (Map)

import Text.Megaparsec
import Text.Megaparsec.Char

import Aoc2020

type Passport = Map String String

parser :: Parser Passport
parser = Map.fromList <$> entry `endBy1` oneOf [' ', '\n']
  where
    entry = do
      key <- takeWhile1P Nothing (\c -> not (isSpace c) && c /= ':')
      _   <- char ':'
      val <- takeWhileP Nothing (not . isSpace)
      pure (key, val)

solve :: (Passport -> Bool) -> [Map String String] -> Int
solve pred = filter pred .> length

main :: (Passport -> Bool) -> IO ()
main pred = mkMain' parser (solve pred)
