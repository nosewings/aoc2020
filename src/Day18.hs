module Day18 where

import Flow
import Aoc2020

data Exp    = Num  Int | Add  Exp Exp | Mul  Exp Exp
data ExpF a = NumF Int | AddF a   a   | MulF a   a
deriving instance Show Exp
deriving instance (Show a) => Show (ExpF a)

cata :: (ExpF a -> a) -> (Exp -> a)
cata alg (Num n)   = alg (NumF n)
cata alg (Add l r) = alg (AddF (cata alg l) (cata alg r))
cata alg (Mul l r) = alg (MulF (cata alg l) (cata alg r))

eval :: Exp -> Int
eval = cata \case
  NumF n   -> n
  AddF l r -> l + r
  MulF l r -> l * r

solve :: [Exp] -> Int
solve = map eval .> sum

main :: Parser Exp -> IO ()
main parser = mkMain parser solve
