module Language.Bitcoin.Test.Opcodes
(
	tests
) where

import Language.Bitcoin.Opcodes
import Language.Bitcoin.Machine
import Language.Bitcoin.Simulator
import Test.HUnit

tests = TestLabel "Opcodes" $ TestList testSimpleOps

simpleOps = [
	  ([], [], [])
	, ([CmdOpcode(OP_FALSE)], [], [[0]])
	, ([CmdOpcode(OP_TRUE)], [], [[1]])
	, ([CmdOpcode(OP_0)], [], [[0]])
	, ([CmdOpcode(OP_1)], [], [[1]])
	, ([CmdOpcode(OP_2)], [], [[2]])
	, ([CmdOpcode(OP_3)], [], [[3]])
	, ([CmdOpcode(OP_4)], [], [[4]])
	, ([CmdOpcode(OP_5)], [], [[5]])
	, ([CmdOpcode(OP_6)], [], [[6]])
	, ([CmdOpcode(OP_7)], [], [[7]])
	, ([CmdOpcode(OP_8)], [], [[8]])
	, ([CmdOpcode(OP_9)], [], [[9]])
	, ([CmdOpcode(OP_10)], [], [[10]])
	, ([CmdOpcode(OP_11)], [], [[11]])
	, ([CmdOpcode(OP_12)], [], [[12]])
	, ([CmdOpcode(OP_13)], [], [[13]])
	, ([CmdOpcode(OP_14)], [], [[14]])
	, ([CmdOpcode(OP_15)], [], [[15]])
	, ([CmdOpcode(OP_16)], [], [[16]])
	, ([CmdOpcode(OP_NOP)], [[23]], [[23]])
	]	
	

testSimpleOps = map runTest simpleOps
	where
		runTest (script, stack, expected) = TestCase $
			case run_simulator (Machine script stack []) of
				Result (Machine _ stack' _) Success -> expected @=? stack' 
				Result (Machine _ stack' _) (Failure _) -> expected @=? stack'
				result -> assertString $ show result
