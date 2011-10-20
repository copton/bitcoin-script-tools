module Language.Bitcoin.Simulator 
(
	run_simulator
) where

import Language.Bitcoin.Machine
import Language.Bitcoin.Opcodes

run_simulator :: Machine -> Either (String, Machine) Stack
run_simulator (Machine [] stack) = Right stack
run_simulator machine =
	case execute machine of
		Left what -> Left (what, machine)
		Right machine' -> run_simulator machine'

execute :: Machine -> Either String Machine
execute (Machine ((CmdOpcode OP_0):ops) stack) = Right $ Machine ops $ (WrdNum 0) : stack
execute (Machine ((CmdOpcode OP_FALSE):ops) stack) = Right $ Machine ops $ (WrdNum 0) : stack
execute _ = error "sorry, not supported yet"
