module Language.Bitcoin.Test.Opcodes
(
	tests
) where

import Language.Bitcoin.Simulator (run_simulator')
import Language.Bitcoin.Types
import Test.HUnit

tests = TestLabel "Opcodes" $ TestList testSimpleOps

simpleOps = [
	  ([], [], [])
	, ([OP_FALSE], [], [[0]])
	, ([OP_TRUE], [], [[1]])
	, ([OP_0], [], [[0]])
	, ([OP_1], [], [[1]])
	, ([OP_2], [], [[2]])
	, ([OP_3], [], [[3]])
	, ([OP_4], [], [[4]])
	, ([OP_5], [], [[5]])
	, ([OP_6], [], [[6]])
	, ([OP_7], [], [[7]])
	, ([OP_8], [], [[8]])
	, ([OP_9], [], [[9]])
	, ([OP_10], [], [[10]])
	, ([OP_11], [], [[11]])
	, ([OP_12], [], [[12]])
	, ([OP_13], [], [[13]])
	, ([OP_14], [], [[14]])
	, ([OP_15], [], [[15]])
	, ([OP_16], [], [[16]])
	, ([OP_NOP], [[23]], [[23]])
	]	
	

testSimpleOps = map runTest simpleOps
	where
		runTest (script, stack, expected) = TestCase $
			case run_simulator' (Machine script stack []) of
				Result Success (Machine _ stack' _) -> expected @=? stack' 
				Result (Failure _) (Machine _ stack' _) -> expected @=? stack'
				result -> assertString $ show result
