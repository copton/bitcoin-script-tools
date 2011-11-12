module Language.Bitcoin.Test.Parser
(
  tests
) where

import Data.Binary (encode)
import Language.Bitcoin.Parser (run_parser)
import Language.Bitcoin.Types
import Test.HUnit
import qualified Data.ByteString as B
import qualified Data.List as List

tests = TestLabel "Parser" $ TestList $ good ++ bad

goodCases = [
    ("OP_FALSE", [CmdOpcode OP_FALSE])
  , ("OP_FALSE ", [CmdOpcode OP_FALSE])
  , (" OP_FALSE", [CmdOpcode OP_FALSE])
  , (" OP_FALSE ; ", [CmdOpcode OP_FALSE])
  , ("OP_FALSE\n", [CmdOpcode OP_FALSE])
  , ("OP_FALSE;", [CmdOpcode OP_FALSE])
  , (" ; \n ;", [])
  , ("OP_FALSE # comment", [CmdOpcode OP_FALSE])
  , ("# comment\nOP_FALSE", [CmdOpcode OP_FALSE])
  , ("OP_FALSE;OP_TRUE", [CmdOpcode OP_FALSE, CmdOpcode OP_TRUE])
  , ("OP_PUSHDATA 01 23", [CmdOpcode $ OP_PUSHDATA Direct (23)])
  , ("OP_PUSHDATA 01 0x23", [CmdOpcode $ OP_PUSHDATA Direct (0x23)])
  , ("OP_PUSHDATA1 06 0x040815162342", [CmdOpcode $ OP_PUSHDATA OneByte (0x40815162342)])
  , ("OP_PUSHDATA2 0006 0x040815162342", [CmdOpcode $ OP_PUSHDATA TwoBytes (0x40815162342)])
  , ("OP_PUSHDATA4 00000006 0x040815162342", [CmdOpcode $ OP_PUSHDATA FourBytes (0x40815162342)])
  , ("DATA 0x040815162342", [DATA 0x40815162342])
  , ("KEY 1", [KEY 1])
  , ("SIG 1", [SIG 1])
  ]

badCases = [
    ("foo", "expecting opcode")
  , ("OP_DOESNOTEXIST", "expecting opcode")
--  , ("OP_PUSHDATA 0;", "expecting hexadecimal digit")
--  , ("OP_PUSHDATA 0x4c;", "OP_PUSHDATA only support up to 75 bytes of data")
--  , ("OP_PUSHDATA1 100 1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111;", "expecting hexadecimal digit")
--  , ("OP_PUSHDATA1 1 2342", "expecting hexadecimal digit")
  ]

good :: [Test]
good = map runTest goodCases
  where
    runTest (code, expected) = TestCase $
      case run_parser "<<test>>" code of
        Left e -> assertFailure e
        Right script -> expected @=? script


bad :: [Test]
bad = map runTest badCases
  where
    runTest (code, expected) = TestCase $
      case run_parser "<<test>>" code of
        Left err -> (last . lines) err @=? expected
        Right _ -> assertFailure "Parser should have failed"
