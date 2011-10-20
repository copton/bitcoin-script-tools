module Language.Bitcoin.Simulator 
(
	run_simulator
) where

import Language.Bitcoin.Machine
import Language.Bitcoin.Opcodes

run_simulator :: Machine -> Result
run_simulator machine@(Machine [] stack _) =
	case stack of
		([1]:_) -> Result machine Success
		_ -> Result machine $ Failure "top stack value is not True"

run_simulator machine@(Machine ((CmdData _):_) _ _) = Result machine $ Error "unexpected data"

run_simulator machine@(Machine ((CmdOpcode op):ops) stack altStack) =
	case simpleOp op stack of
		Left what -> Result machine $ Error what
		Right (stack') -> run_simulator $ Machine ops stack' altStack


simpleOp :: Opcode -> Stack -> Either String Stack
simpleOp OP_FALSE stack = Right $ [0] : stack
simpleOp OP_TRUE stack = Right $ [1] : stack
simpleOp OP_0 stack = Right $ [0] : stack
simpleOp OP_1 stack = Right $ [1] : stack
simpleOp OP_2 stack = Right $ [2] : stack
simpleOp OP_3 stack = Right $ [3] : stack
simpleOp OP_4 stack = Right $ [4] : stack
simpleOp OP_5 stack = Right $ [5] : stack
simpleOp OP_6 stack = Right $ [6] : stack
simpleOp OP_7 stack = Right $ [7] : stack
simpleOp OP_8 stack = Right $ [8] : stack
simpleOp OP_9 stack = Right $ [9] : stack
simpleOp OP_10 stack = Right $ [10] : stack
simpleOp OP_11 stack = Right $ [11] : stack
simpleOp OP_12 stack = Right $ [12] : stack
simpleOp OP_13 stack = Right $ [13] : stack
simpleOp OP_14 stack = Right $ [14] : stack
simpleOp OP_15 stack = Right $ [15] : stack
simpleOp OP_16 stack = Right $ [16] : stack
simpleOp OP_NOP stack = Right stack
simpleOp op _ = Left $ "sorry, opcode " ++ show op ++ " is not implemented yet"
