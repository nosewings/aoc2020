module Day17 where

import GHC.TypeNats
import Data.Proxy
import Text.Megaparsec

import Aoc2020

parser :: Parser [Bool]
parser = do
  cs <- takeWhile1P Nothing (\c -> c /= '\n' && c /= '\r')
  pure $ map (\case '.' -> False; '#' -> True) cs

main :: (forall k proxy. (KnownNat k) => proxy k -> [[Bool]] -> Int) -> IO ()
main solve = mkMain parser (solve (Proxy :: Proxy 6))
