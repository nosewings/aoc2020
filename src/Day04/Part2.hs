module Main where

import Flow

import Data.Map.Strict ((!?))
import Data.Maybe

import Text.Megaparsec
import Text.Megaparsec.Char
import Text.Megaparsec.Char.Lexer

import Aoc2020
import Day04 hiding (main)
import Day04 qualified

data Hgt = Cm Int | In Int
deriving instance (Show Hgt)

isValid :: Passport -> Bool
isValid m = all (\(key, pred) -> m !? key |> maybe False pred) [ ("byr", isValidByr)
                                                               , ("iyr", isValidIyr)
                                                               , ("eyr", isValidEyr)
                                                               , ("hgt", isValidHgt)
                                                               , ("hcl", isValidHcl)
                                                               , ("ecl", isValidEcl)
                                                               , ("pid", isValidPid)
                                                               ]
  where
    isValidByr = parseMaybe (byr <* eof) .> maybe False \n -> 1920 <= n && n <= 2002
      where
        byr :: Parser Int
        byr = decimal
    isValidIyr = parseMaybe (iyr <* eof) .> maybe False \n -> 2010 <= n && n <= 2020
      where
        iyr :: Parser Int
        iyr = decimal
    isValidEyr = parseMaybe (eyr <* eof) .> maybe False \n -> 2020 <= n && n <= 2030
      where
        eyr :: Parser Int
        eyr = decimal
    isValidHgt = parseMaybe (hgt <* eof) .> maybe False \case
      Cm n -> 150 <= n && n <= 193
      In n -> 59 <= n && n <= 76
      where
        hgt :: Parser Hgt
        hgt = do
          n <- decimal
          (Cm n <$ "cm") <|> (In n <$ "in")
    isValidHcl :: String -> Bool
    isValidHcl = parseMaybe (hcl *> eof) .> isJust
      where
        hcl :: Parser Int
        hcl = do
          _ <- char '#'
          _ <- lookAhead (takeP Nothing 6 *> eof)
          hexadecimal
    isValidEcl :: String -> Bool
    isValidEcl = parseMaybe (ecl <* eof) .> isJust
      where
        ecl :: Parser String
        ecl = "amb" <|> "blu" <|> "brn" <|> "gry" <|> "grn" <|> "hzl" <|> "oth"
    isValidPid :: String -> Bool
    isValidPid = parseMaybe (pid <* eof) .> isJust
      where
        pid :: Parser Int
        pid = do
          _ <- lookAhead (takeP Nothing 9 *> eof)
          decimal

main :: IO ()
main = Day04.main isValid
