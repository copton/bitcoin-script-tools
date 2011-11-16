module Language.Bitcoin.Printer
-- export {{{1
(
  run_printer
) where

-- import {{{1
import Language.Bitcoin.Types

run_printer :: Program -> Code -- {{{1
run_printer program = printProgram program ""

printProgram :: Program -> ShowS
printProgram [] = id
printProgram ((OP_PUSHDATA Implicit _ data_):rest) =
  showString "OP_PUSHDATA " . shows data_ . showChar '\n' . printProgram rest
printProgram ((OP_PUSHDATA OneByte len data_):rest) =
  showString "OP_PUSHDATA1 " . shows len . showChar ' ' . shows data_ . showChar '\n'. printProgram rest
printProgram ((OP_PUSHDATA TwoBytes len data_):rest) =
  showString "OP_PUSHDATA2 " . shows len . showChar ' ' . shows data_ . showChar '\n' . printProgram rest
printProgram ((OP_PUSHDATA FourBytes len data_):rest) =
  showString "OP_PUSHDATA4 " . shows len . showChar ' ' . shows data_ . showChar '\n' . printProgram rest
printProgram (opcode:rest) = shows opcode . showChar '\n' . printProgram rest
