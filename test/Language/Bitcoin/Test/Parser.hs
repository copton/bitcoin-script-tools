module Language.Bitcoin.Test.Parser
(
  tests
) where

import Data.Binary (encode)
import Language.Bitcoin.Parser (run_parser)
import Language.Bitcoin.Types
import Test.HUnit
import qualified Data.ByteString.Lazy as B
import qualified Data.List as List

import Debug.Trace (trace)
tests = TestLabel "Parser" $ TestList $ good ++ bad

goodCases = [
    ("OP_FALSE\n", [CmdOpcode OP_FALSE])
  , ("OP_FALSE;", [CmdOpcode OP_FALSE])
  , (" OP_FALSE;", [CmdOpcode OP_FALSE])
  , ("OP_FALSE;OP_TRUE;", [CmdOpcode OP_FALSE, CmdOpcode OP_TRUE])
  , ("OP_FALSE\nOP_TRUE;", [CmdOpcode OP_FALSE, CmdOpcode OP_TRUE])
  , ("PUSH 23;", [CmdOpcode $ PUSH 0x23])
  , ("OP_PUSHDATA1 6 040815162342;", [CmdOpcode $ OP_PUSHDATA1 6 0x40815162342])
  , ("OP_PUSHDATA2 6 040815162342;", [CmdOpcode $ OP_PUSHDATA2 6 0x40815162342])
  , ("OP_PUSHDATA4 6 040815162342;", [CmdOpcode $ OP_PUSHDATA4 6 0x40815162342])
  , ("DATA 040815162342;", [DATA 0x40815162342])
  , ("KEY 1;", [KEY 1])
  , ("SIG 1;", [SIG 1])
  ]

badCases = [
  "foo;",
  "OP_DOESNOTEXIST;",
  "PUSH 0;",
  "PUSH 76;",
  "OP_PUSHDATA1 100 1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111;",
  "OP_PUSHDATA1 1 2342"
  ]

good :: [Test]
good = map runTest goodCases
  where
    runTest (code, expected) = TestCase $
      case run_parser "<<test>>" code of
        Left e -> assertString e
        Right script -> expected @=? script


bad :: [Test]
bad = map runTest badCases
  where
    runTest code = TestCase $
      case run_parser "<<test>>" code of
        Left err -> putStrLn $ infoString code err
        Right _ -> assertString "Parser should have failed"

infoString :: String -> String -> String
infoString code err =
  "'" ++ code ++ "' -> " ++ (List.reverse $ takeWhile (/= '\n') $ List.reverse err)

