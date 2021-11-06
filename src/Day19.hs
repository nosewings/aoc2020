module Day19 where

import Flow

import Control.Monad

import Data.IntMap.Lazy (IntMap, (!))
import Data.IntMap.Lazy qualified as IntMap
import Data.List
import Data.Maybe

import Text.Megaparsec hiding (count, parseMaybe)
import Text.Megaparsec.Char
import Text.Megaparsec.Char.Lexer

import Text.ParserCombinators.ReadP (ReadP)
import Text.ParserCombinators.ReadP qualified as ReadP

import Aoc2020

data RulePayload = Terminal Char | Nonterminal [[Int]]
deriving instance Show RulePayload

data Rule = Rule Int RulePayload
deriving instance Show Rule

data Input = Input
  { rules    :: [Rule]
  , messages :: [String]
  }
deriving instance Show Input

parser :: Parser Input
parser = input where
  input = do
    rules    <- rule `endBy` eol
    _        <- eol
    messages <- message `endBy` eol
    pure $ Input rules messages
  rule = do
    name    <- decimal
    _       <- ": "
    payload <- terminal <|> nonterminal
    pure $ Rule name payload
  terminal = Terminal <$> between (char '"') (char '"') anySingle
  nonterminal = Nonterminal <$> many (decimal <* hspace) `sepBy` (char '|' *> hspace)
  message = takeWhile1P Nothing (\c -> c /= '\n' && c /= '\r')

parseMaybe :: ReadP a -> String -> Maybe a
parseMaybe p = ReadP.readP_to_S p .> find (snd .> null) .> fmap fst

mkParsers :: IntMap RulePayload -> IntMap (ReadP ())
mkParsers rules = result
  where
    result = fmap mkParser rules
    mkParser (Terminal c)     = c |> ReadP.char |> void
    mkParser (Nonterminal xs) = xs |> map (mapM_ (result !)) |> choice

rules2intmap :: [Rule] -> IntMap RulePayload
rules2intmap rules = IntMap.fromList [(n, p) | Rule n p <- rules]

solve :: (IntMap RulePayload -> IntMap RulePayload) -> Input -> Int
solve f Input{..} = messages |> map (parseMaybe parsers) |> count isJust
 where
   parsers = rules |> rules2intmap |> f |> mkParsers |> (! 0)

main :: (IntMap RulePayload -> IntMap RulePayload) -> IO ()
main f = mkMain'' parser (solve f)
