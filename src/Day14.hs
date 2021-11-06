module Day14 where

import Flow

import Control.Lens hiding ((.>), (|>))

import Data.Bits
import Data.Foldable
import Data.Map.Strict (Map)
import Data.Monoid

import Text.Megaparsec hiding (State)
import Text.Megaparsec.Char
import Text.Megaparsec.Char.Lexer

import Aoc2020

type Number = [Bool]

int2number :: Int -> Number
int2number n = [35, 34 .. 0] |> map (testBit n)

number2int :: Number -> Int
number2int = zip [35, 34 .. 0] .> foldMap' (\(i, b) -> Sum $ if b then 2^i else 0) .> getSum

type Bitmask = [Maybe Bool]

data Statement = Mask Bitmask | Mem Number Number
deriving instance Show Statement

type Program = [Statement]

parser :: Parser Statement
parser = mask <|> mem where
  mask = Mask <$> ("mask" *> equals *> bitmask)
  mem = do
    _ <- "mem"
    _ <- char '['
    i <- decimal
    _ <- char ']'
    _ <- equals
    n <- decimal
    pure (Mem (int2number i) (int2number n))
  bitmask = do
    cs <- takeWhile1P Nothing (\c -> c == '0' || c == '1' || c == 'X')
    pure $ map (\case '0' -> Just False; '1' -> Just True; 'X' -> Nothing) cs
  equals = hspace *> char '=' <* hspace

type Memory = Map Number Number

data State = State
  { _mask :: Bitmask
  , _mem  :: Memory
  }
makeLenses ''State
deriving instance Show State

emptyState :: State
emptyState = State
  { _mask = replicate 36 Nothing
  , _mem  = []
  }

execute :: (Statement -> State -> State) -> [Statement] -> State
execute step = foldl' (flip step) emptyState

solve :: (Statement -> State -> State) -> [Statement] -> Int
solve step = execute step .> view mem .> foldMap' (number2int .> Sum) .> getSum

main :: (Statement -> State -> State) -> IO ()
main step = mkMain parser (solve step)
