module Language.Bitcoin.Text
-- export {{{1
(
  print_result
) where

-- import {{{1
import Language.Bitcoin.Types
import Data.Char (intToDigit)
import Data.List (intersperse)
import Data.Word (Word8)
import qualified Data.ByteString as B

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

print_data :: B.ByteString -> String
print_data data_ = concat $ map print_hex $ B.unpack data_

print_stack :: Stack -> String
print_stack stack = (show $ head stack) ++ "," ++ (print_stack $ tail stack)

print_opcode :: Opcode -> String
print_opcode (OP_PUSHDATA pushType data_) = "OP_PUSHDATA " ++ show pushType ++ print_data data_
print_opcode op = show op

print_hex :: Word8 -> String
print_hex value =
  let (q, r) = quotRem value 16 in
  [intToDigit (fromIntegral q), intToDigit (fromIntegral r)]

