module Language.Bitcoin.Test.Preprocessor
(
  tests
) where

import Language.Bitcoin.Parser (run_parser)
import Language.Bitcoin.Preprocessor (run_preprocessor)
import Language.Bitcoin.Types
import Test.HUnit

key = id
sig = negate

testCases = [
    (
      "OP_FALSE", ([OP_FALSE], [])
  ),(
      "KEY 01", (
        [OP_PUSHDATA Direct 64 (key 1)],
        [Keypair (key 1) (sig 1)]
      )
  ),(
      "KEY 01;KEY 01", (
        [OP_PUSHDATA Direct 64 (key 1), OP_PUSHDATA Direct 64 (key 1)],
        [Keypair (key 1) (sig 1)]
      )
  ),(
      "KEY 01;SIG 01", (
        [OP_PUSHDATA Direct 64 (key 1), OP_PUSHDATA Direct 64 (sig 1)],
        [Keypair (key 1) (sig 1)]
      )
  ),(
      "KEY 01;KEY 02", (
        [OP_PUSHDATA Direct 64 (key 1), OP_PUSHDATA Direct 64 (key 2)],
        [Keypair (key 1) (sig 1), Keypair (key 2) (sig 2)]
      )
  ),(
      "DATA 0x1234", (
        [OP_PUSHDATA Direct 2 0x1234],
        []
      )
  ),(
      "DATA 0x11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111;", (
        [OP_PUSHDATA OneByte 118 0x11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111],
        []
      )    
  )
  ]

tests = TestLabel "Preprocessor" $ TestList $ map runTest testCases

runTest :: (Code, (Program, Keyring)) -> Test
runTest (input, expected) = TestCase $ 
  case run_parser "<<test>>" input of
    Left e -> assertFailure e
    Right script -> 
      expected @=? run_preprocessor script
