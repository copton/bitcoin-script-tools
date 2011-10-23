module Language.Bitcoin.Interpreter
(
	run_interpreter, run_interpreter'
) where

import Language.Bitcoin.Types
import Language.Bitcoin.Utils (b2i, i2b)

run_interpreter :: Program -> Keyring -> Either String Result
run_interpreter program keyring =
  case run_interpreter' (Machine program keyring [] [])  of
    result@(Result (Error _) _) -> Left $ show result
    result -> Right result

run_interpreter' :: Machine -> Result
run_interpreter' machine@(Machine [] _ stack _) =
  if checkSuccess stack
    then Result Success machine
    else Result (Failure "top stack value is not True") machine
  where
    checkSuccess (x:_) =
      case b2i x of
        Left _ -> False
        Right value -> value == 1
    checkSuccess _ = False

--run_interpreter' (Machine ((PASTE _ data_):rest) stack altStack) =
--	run_interpreter' (Machine rest (Raw data_ : stack) altStack)

run_interpreter' machine@(Machine (op:rest) keyring stack altStack) =
	case simpleOp op stack of
		Left what -> Result (Error what) machine
		Right (stack') -> run_interpreter' (Machine rest keyring stack' altStack)


simpleOp :: Opcode -> Stack -> Either String Stack
simpleOp OP_FALSE stack = Right $ i2b  0 : stack
simpleOp OP_TRUE  stack = Right $ i2b  1 : stack
simpleOp OP_0     stack = Right $ i2b  0 : stack
simpleOp OP_1     stack = Right $ i2b  1 : stack
simpleOp OP_2     stack = Right $ i2b  2 : stack
simpleOp OP_3     stack = Right $ i2b  3 : stack
simpleOp OP_4     stack = Right $ i2b  4 : stack
simpleOp OP_5     stack = Right $ i2b  5 : stack
simpleOp OP_6     stack = Right $ i2b  6 : stack
simpleOp OP_7     stack = Right $ i2b  7 : stack
simpleOp OP_8     stack = Right $ i2b  8 : stack
simpleOp OP_9     stack = Right $ i2b  9 : stack
simpleOp OP_10    stack = Right $ i2b 10 : stack
simpleOp OP_11    stack = Right $ i2b 11 : stack
simpleOp OP_12    stack = Right $ i2b 12 : stack
simpleOp OP_13    stack = Right $ i2b 13 : stack
simpleOp OP_14    stack = Right $ i2b 14 : stack
simpleOp OP_15    stack = Right $ i2b 15 : stack
simpleOp OP_16    stack = Right $ i2b 16 : stack
simpleOp OP_NOP   stack = Right stack
simpleOp op _ = Left $ "sorry, opcode " ++ show op ++ " is not implemented yet"
