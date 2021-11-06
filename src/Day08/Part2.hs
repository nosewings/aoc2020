module Main where

import Flow

import Control.Lens hiding ((.>), (|>), op)

import Data.List
import Data.Maybe
import Data.Sequence (Seq)
import Data.Sequence qualified as Seq

import Day08 hiding (main)
import Day08 qualified

flipOp :: Op -> Op
flipOp Acc = Acc
flipOp Jmp = Nop
flipOp Nop = Jmp

flipInstr :: Instr -> Instr
flipInstr = over op flipOp

solve :: Seq Instr -> Int
solve instrs =
  instrs |>
  execInstrs |>
  view history |>
  filter (Seq.index instrs .> view op .> (/= Acc)) |>
  map (Seq.adjust' flipInstr ?? instrs) |>
  map execInstrs |>
  find (not . hasLooped) |>
  fromJust |>
  view acc

main :: IO ()
main = Day08.main solve
