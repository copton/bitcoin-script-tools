module Language.Bitcoin.Simulator 
(
	run_simulator, run_simulator'
) where

import Language.Bitcoin.Types

run_simulator :: Program -> Either String Result
run_simulator program =
  case run_simulator' (Machine program [] []) of
    result@(Result (Error _) _) -> Left $ show result
    result -> Right result

run_simulator' :: Machine -> Result
run_simulator' machine@(Machine [] stack _) =
  if checkSuccess stack
    then Result Success machine
    else Result (Failure "top stack value is not True") machine
  where
    checkSuccess [] = False
    checkSuccess (Raw x:_) = x == 1
    checkSuccess _ = False

--run_simulator' (Machine ((PASTE _ data_):rest) stack altStack) =
--	run_simulator' (Machine rest (Raw data_ : stack) altStack)

run_simulator' machine@(Machine (op:rest) stack altStack) =
	case simpleOp op stack of
		Left what -> Result (Error what) machine
		Right (stack') -> run_simulator' (Machine rest stack' altStack)


simpleOp :: Opcode -> Stack -> Either String Stack
simpleOp OP_FALSE stack = Right $ Raw  0 : stack
simpleOp OP_TRUE  stack = Right $ Raw  1 : stack
simpleOp OP_0     stack = Right $ Raw  0 : stack
simpleOp OP_1     stack = Right $ Raw  1 : stack
simpleOp OP_2     stack = Right $ Raw  2 : stack
simpleOp OP_3     stack = Right $ Raw  3 : stack
simpleOp OP_4     stack = Right $ Raw  4 : stack
simpleOp OP_5     stack = Right $ Raw  5 : stack
simpleOp OP_6     stack = Right $ Raw  6 : stack
simpleOp OP_7     stack = Right $ Raw  7 : stack
simpleOp OP_8     stack = Right $ Raw  8 : stack
simpleOp OP_9     stack = Right $ Raw  9 : stack
simpleOp OP_10    stack = Right $ Raw 10 : stack
simpleOp OP_11    stack = Right $ Raw 11 : stack
simpleOp OP_12    stack = Right $ Raw 12 : stack
simpleOp OP_13    stack = Right $ Raw 13 : stack
simpleOp OP_14    stack = Right $ Raw 14 : stack
simpleOp OP_15    stack = Right $ Raw 15 : stack
simpleOp OP_16    stack = Right $ Raw 16 : stack
simpleOp OP_NOP   stack = Right stack
simpleOp op _ = Left $ "sorry, opcode " ++ show op ++ " is not implemented yet"
