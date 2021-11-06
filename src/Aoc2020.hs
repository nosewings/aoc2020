module Aoc2020 where

import Flow

import Control.Applicative
import Control.Lens hiding ((.>), (|>))
import Control.Monad.State

import Data.Bool
import Data.Foldable
import Data.List
import Data.Maybe
import Data.Monoid
import Data.Void

import Text.Megaparsec hiding (State)
import Text.Megaparsec.Char

type Parser a = Parsec Void String a

mkMain :: Parser a -> ([a] -> Int) -> IO ()
mkMain parser solve = mkMainStr parser (solve .> show)

mkMainStr :: Parser a -> ([a] -> String) -> IO ()
mkMainStr parser solve = interact $
  parseMaybe (parser `endBy` char '\n') .>
  fromJust .>
  solve .>
  (++ "\n")

mkMain' :: Parser a -> ([a] -> Int) -> IO ()
mkMain' parser solve = interact $
  parseMaybe (parser `sepBy` char '\n') .>
  fromJust .>
  solve .>
  show .>
  (++ "\n")

mkMain'' :: Parser a -> (a -> Int) -> IO ()
mkMain'' parser solve = mkMainStr'' parser (solve .> show)

mkMainStr'' :: Parser a -> (a -> String) -> IO ()
mkMainStr'' parser solve = interact $
  parseMaybe parser .>
  fromJust .>
  solve .>
  (++ "\n")

mkMain''' :: Parser a -> (a -> IO String) -> IO ()
mkMain''' parser solve = do
  input <- getContents
  let Just x = parseMaybe parser input
  output <- solve x
  putStrLn output

transpose' :: [[a]] -> [[a]]
transpose' = traverse ZipList .> getZipList

windows :: Int -> [a] -> [[a]]
windows n = tails .> take n .> transpose'

windows2 :: [a] -> [(a, a)]
windows2 = windows 2 .> map (\[x, y] -> (x, y))

count :: (Foldable f) => (a -> Bool) -> f a -> Int
count p = foldMap' (p .> bool 0 1) .> getSum

findFixpoint :: (Eq a) => (a -> a) -> a -> Maybe a
findFixpoint f = iterate f .> windows2 .> find (uncurry (==)) .> fmap fst

-- | Compute the diagonal of a list of lists.
diagonal :: [[a]] -> [a]
diagonal []          = []
diagonal ([]:_)      = []
diagonal ((x:_):xss) = x : diagonal [drop 1 xs | xs <- xss]

pullMaybe :: (s -> Maybe (a, s)) -> (s -> (Maybe a, s))
pullMaybe f s = case f s of
  Nothing      -> (Nothing, s)
  Just (x, s') -> (Just x, s')

pullMaybeS :: (s -> Maybe (a, s)) -> State s (Maybe a)
pullMaybeS = pullMaybe .> state

(=<>) :: (MonadState s m, Semigroup a) => ASetter' s a -> a -> m ()
l =<> a = modify (over l (a <>))

iterateM :: (Monad m) => (a -> m a) -> Int -> a -> m [a]
iterateM f n x = x |> return |> iterate (>>= f) |> take n |> sequence
