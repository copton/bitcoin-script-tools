module Language.Bitcoin.Test.Simulator
(
	tests
) where

import Language.Bitcoin.Simulator (run_simulator')
import Language.Bitcoin.Types
import Test.HUnit

tests = TestLabel "Simulator" $ TestList testSimpleOps

simpleOps = [
	  ([], [], [])
	, ([OP_FALSE], [], [Raw  0])
	, ([OP_TRUE],  [], [Raw  1])
	, ([OP_0],     [], [Raw  0])
	, ([OP_1],     [], [Raw  1])
	, ([OP_2],     [], [Raw  2])
	, ([OP_3],     [], [Raw  3])
	, ([OP_4],     [], [Raw  4])
	, ([OP_5],     [], [Raw  5])
	, ([OP_6],     [], [Raw  6])
	, ([OP_7],     [], [Raw  7])
	, ([OP_8],     [], [Raw  8])
	, ([OP_9],     [], [Raw  9])
	, ([OP_10],    [], [Raw 10])
	, ([OP_11],    [], [Raw 11])
	, ([OP_12],    [], [Raw 12])
	, ([OP_13],    [], [Raw 13])
	, ([OP_14],    [], [Raw 14])
	, ([OP_15],    [], [Raw 15])
	, ([OP_16],    [], [Raw 16])
	, ([OP_NOP], [Raw 23], [Raw 23])
	]	
	

testSimpleOps = map runTest simpleOps
	where
		runTest (script, stack, expected) = TestCase $
			case run_simulator' (Machine script stack []) of
				Result Success (Machine _ stack' _) -> expected @=? stack' 
				Result (Failure _) (Machine _ stack' _) -> expected @=? stack'
				result -> assertString $ show result
