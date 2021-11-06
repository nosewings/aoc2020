module Day02 where

import Flow

import Data.Char

import Text.Megaparsec hiding (parse)
import Text.Megaparsec.Char
import Text.Megaparsec.Char.Lexer

import Aoc2020

data Password = Password Int Int Char String
deriving instance Show Password

parser :: Parser Password
parser = do
  l <- decimal
  _ <- char '-'
  u <- decimal
  _ <- char ' '
  c <- anySingle
  _ <- string ": "
  t <- takeWhileP Nothing (not . isSpace)
  pure $ Password l u c t

solve :: (Password -> Bool) -> [Password] -> Int
solve pred = filter pred .> length

main :: (Password -> Bool) -> IO ()
main pred = mkMain parser (solve pred)
