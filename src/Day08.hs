module Day08 where

import Flow

import Control.Lens hiding ((.>), (|>), op)
import Control.Monad.Reader
import Control.Monad.State
import Control.Monad.Loops

import Data.Sequence (Seq)
import Data.Sequence qualified as Seq

import Text.Megaparsec hiding (State)
import Text.Megaparsec.Char
import Text.Megaparsec.Char.Lexer

import Aoc2020

data Op = Acc | Jmp | Nop
deriving instance Show Op
deriving instance Eq   Op

data Instr = Instr
  { _op  :: Op
  , _arg :: Int
  }
makeLenses ''Instr
deriving instance Show Instr

parser :: Parser Instr
parser = Instr <$> op <*> number
  where
    op = ((Acc <$ "acc") <|> (Jmp <$ "jmp") <|> (Nop <$ "nop")) <* hspace
    number = do
      s <- sign
      n <- decimal
      return (s * n)
    sign = (1 <$ char '+') <|> (-1 <$ char '-')

data ExecutionState = ExecutionState
  { _pc      :: Int
  , _acc     :: Int
  , _history :: [Int]
  }
makeLenses ''ExecutionState
deriving instance Show ExecutionState

hasLooped :: ExecutionState -> Bool
hasLooped s = s^.pc `elem` s^.history

type Execution a = ReaderT (Seq Instr) (State ExecutionState) a

execExecution :: Seq Instr -> ExecutionState -> Execution a -> ExecutionState
execExecution r s m = execState (runReaderT m r) s

currentInstruction :: Execution Instr
currentInstruction = asks Seq.index <*> gets (view pc)

isDone :: Execution Bool
isDone = do
  p <- gets (view pc)
  l <- asks length
  return (p == l)

step :: Execution ()
step = do
  p <- gets (view pc)
  history %= (p:)
  i <- currentInstruction
  case i^.op of
    Acc -> do
      acc += i^.arg
      pc  += 1
    Jmp -> pc += i^.arg
    Nop -> pc += 1

execInstrs :: Seq Instr -> ExecutionState
execInstrs instrs = execExecution instrs initialState action
  where
    initialState = ExecutionState
      { _pc      = 0
      , _acc     = 0
      , _history = []
      }
    action = whileM_ (not <$> ((||) <$> isDone <*> gets hasLooped)) step

main :: (Seq Instr -> Int) -> IO ()
main solve = mkMain parser (Seq.fromList .> solve)
