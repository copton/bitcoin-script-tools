module Language.Bitcoin.Test.Opcodes
(
	tests
) where

import Language.Bitcoin.Opcodes
import Language.Bitcoin.Machine
import Language.Bitcoin.Simulator
import Test.HUnit

tests = TestLabel "Opcodes" $ TestList $ map runTest testCases

testCases = [([CmdOpcode(OP_0)], [], [WrdNum 0])]

runTest :: (Script, Stack, Stack) -> Test
runTest (script, stack, expected) = TestCase $
	case run_simulator (Machine script stack) of
		Left (what, machine) -> assertString $ show what ++ "\n" ++ show machine
		Right stack' -> expected @=? stack'
