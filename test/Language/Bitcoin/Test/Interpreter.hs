module Language.Bitcoin.Test.Interpreter
(
	tests
) where

import Language.Bitcoin.Interpreter (run_interpreter')
import Language.Bitcoin.Types
import Language.Bitcoin.Utils
import Language.Bitcoin.Text (print_result)
import Test.HUnit

tests = TestLabel "Simulator" $ TestList testSimpleOps

simpleOps = [
	  ([], [], [])
	, ([OP_FALSE], [], [i2b  0])
	, ([OP_FALSE], [], [i2b  0])
	, ([OP_TRUE],  [], [i2b  1])
	, ([OP_0],     [], [i2b  0])
	, ([OP_1],     [], [i2b  1])
	, ([OP_2],     [], [i2b  2])
	, ([OP_3],     [], [i2b  3])
	, ([OP_4],     [], [i2b  4])
	, ([OP_5],     [], [i2b  5])
	, ([OP_6],     [], [i2b  6])
	, ([OP_7],     [], [i2b  7])
	, ([OP_8],     [], [i2b  8])
	, ([OP_9],     [], [i2b  9])
	, ([OP_10],    [], [i2b 10])
	, ([OP_11],    [], [i2b 11])
	, ([OP_12],    [], [i2b 12])
	, ([OP_13],    [], [i2b 13])
	, ([OP_14],    [], [i2b 14])
	, ([OP_15],    [], [i2b 15])
	, ([OP_16],    [], [i2b 16])
	, ([OP_NOP], [i2b 23], [i2b 23])
	]	
	

testSimpleOps = map runTest simpleOps
	where
		runTest (script, stack, expected) = TestCase $
			case run_interpreter' (Machine script [] stack []) of
				Result Success (Machine _ _ stack' _) -> expected @=? stack' 
				Result (Failure _) (Machine _ _ stack' _) -> expected @=? stack'
				result -> assertFailure $ print_result result
