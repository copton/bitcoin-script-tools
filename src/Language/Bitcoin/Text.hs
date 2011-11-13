module Language.Bitcoin.Text
-- export {{{1
(
  print_result
) where

-- import {{{1
import Language.Bitcoin.Types
import Data.List (intersperse)

print_result :: Result -> String -- {{{1
print_result (Result code machine) = print_result_code code ++ "\n" ++ print_machine machine

print_result_code :: ResultCode -> String
print_result_code Success = "Bitcoin script completed successfully."
print_result_code (Failure what) = "Bitcoin script failed: " ++ what
print_result_code (Error what) = "Bitcoin script is illegal: " ++ what

print_machine :: Machine -> String
print_machine (Machine program _ stack altStack) =
  concat $ intersperse "\n\t" $ [
    "Machine:",
    "program: " ++ print_program program,
    "stack: "  ++ print_stack stack,
    "alt stack: " ++ print_stack altStack
    ]

printList :: (a -> String) -> [a] -> String
printList f lst = "(" ++ (concat $ intersperse ", " $ map f lst) ++ ")"

print_program :: Program -> String
print_program program = printList print_opcode program

print_stack :: Stack -> String
print_stack stack = (show $ head stack) ++ "," ++ (print_stack $ tail stack)

print_opcode :: Opcode -> String
print_opcode (OP_PUSHDATA pushType _ data_) = "OP_PUSHDATA " ++ show pushType ++ show data_
print_opcode op = show op
