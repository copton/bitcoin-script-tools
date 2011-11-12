module Language.Bitcoin.Test.Interpreter
(
	tests
) where

import Language.Bitcoin.Interpreter (exec)
import Language.Bitcoin.Types
import Language.Bitcoin.Text (print_result)
import Test.HUnit

tests = TestLabel "Simulator" $ TestList testSimpleOps

simpleOps = [
	  ([], [], [])
	, ([OP_FALSE], [], [ 0])
	, ([OP_FALSE], [], [ 0])
	, ([OP_TRUE],  [], [ 1])
	, ([OP_0],     [], [ 0])
	, ([OP_1],     [], [ 1])
	, ([OP_2],     [], [ 2])
	, ([OP_3],     [], [ 3])
	, ([OP_4],     [], [ 4])
	, ([OP_5],     [], [ 5])
	, ([OP_6],     [], [ 6])
	, ([OP_7],     [], [ 7])
	, ([OP_8],     [], [ 8])
	, ([OP_9],     [], [ 9])
	, ([OP_10],    [], [10])
	, ([OP_11],    [], [11])
	, ([OP_12],    [], [12])
	, ([OP_13],    [], [13])
	, ([OP_14],    [], [14])
	, ([OP_15],    [], [15])
	, ([OP_16],    [], [16])
	, ([OP_NOP], [23], [23])
	]	
	

testSimpleOps = map runTest simpleOps
	where
		runTest (script, stack, expected) = TestCase $
			case exec (Machine script [] stack []) of
				Result Success (Machine _ _ stack' _) -> expected @=? stack' 
				Result (Failure _) (Machine _ _ stack' _) -> expected @=? stack'
				result -> assertFailure $ print_result result
