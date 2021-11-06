module Day12 where

import Flow

import Control.Lens hiding ((.>))

import Data.Foldable

import Text.Megaparsec
import Text.Megaparsec.Char
import Text.Megaparsec.Char.Lexer

import Aoc2020

data Cardinal = N | E | S | W
deriving instance Show Cardinal
deriving instance Enum Cardinal

data Relative = L | R
deriving instance Show Relative

relative2int :: Relative -> Int
relative2int L = -1
relative2int R = 1

data Action = Move Cardinal | Turn Relative | F
deriving instance Show Action

isTurn :: Action -> Bool
isTurn (Turn _) = True
isTurn _        = False

data Command = Command Action Int
deriving instance Show Command

parser :: Parser Command
parser = do
  a <- action
  n <- decimal
  let n' = if isTurn a then n `div` 90 else n
  return $ Command a n'
  where
    action =
      Move N <$ char 'N' <|>
      Move E <$ char 'E' <|>
      Move S <$ char 'S' <|>
      Move W <$ char 'W' <|>
      Turn L <$ char 'L' <|>
      Turn R <$ char 'R' <|>
      F      <$ char 'F'

data Location = Location
  { _x :: Int
  , _y :: Int
  }
makeLenses ''Location
deriving instance Show Location

instance Num Location where
  Location x y + Location x' y' = Location (x + x') (y + y')
  Location x y - Location x' y' = Location (x - x') (y - y')
  Location x y * Location x' y' = Location (x * x') (y * y')
  negate (Location x y) = Location (negate x) (negate y)
  abs (Location x y) = Location (abs x) (abs y)
  signum _ = undefined
  fromInteger n = Location (fromInteger n) (fromInteger n)

norm :: Location -> Int
norm l = abs (l^.x) + abs (l^.y)

updateLocation :: Cardinal -> Int -> Location -> Location
updateLocation N n = y +~ n
updateLocation S n = y -~ n
updateLocation E n = x +~ n
updateLocation W n = x -~ n

solve :: (Command -> a -> a) -> a -> (a -> Location) -> [Command] -> Int
solve op e proj = foldl' (flip op) e .> proj .> norm

main :: (Command -> a -> a) -> a -> (a -> Location) -> IO ()
main op e proj = mkMain parser (solve op e proj)
