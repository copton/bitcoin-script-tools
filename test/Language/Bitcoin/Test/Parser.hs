module Language.Bitcoin.Test.Parser
(
  tests
) where

import Data.Binary (encode)
import Language.Bitcoin.Parser (run_parser)
import Language.Bitcoin.Types
import Language.Bitcoin.Utils (bs)
import Test.HUnit
import qualified Data.ByteString as B
import qualified Data.List as List

tests = TestLabel "Parser" $ TestList $ good ++ bad

goodCases = [
    ("OP_FALSE", [CmdOpcode OP_FALSE])
  , ("OP_FALSE\n", [CmdOpcode OP_FALSE])
  , ("OP_FALSE;", [CmdOpcode OP_FALSE])
  , (" OP_FALSE ; ", [CmdOpcode OP_FALSE])
  , (" ; \n ;", [])
  , ("OP_FALSE;OP_TRUE", [CmdOpcode OP_FALSE, CmdOpcode OP_TRUE])
  , ("OP_PUSHDATA 01 23", [CmdOpcode $ OP_PUSHDATA Direct (bs 0x23)])
  , ("OP_PUSHDATA1 06 040815162342", [CmdOpcode $ OP_PUSHDATA OneByte (bs 0x40815162342)])
  , ("OP_PUSHDATA2 0006 040815162342", [CmdOpcode $ OP_PUSHDATA TwoBytes (bs 0x40815162342)])
  , ("OP_PUSHDATA4 00000006 040815162342", [CmdOpcode $ OP_PUSHDATA FourBytes (bs 0x40815162342)])
  , ("DATA 040815162342", [DATA $ bs 0x40815162342])
  , ("KEY 01", [KEY 1])
  , ("SIG 01", [SIG 1])
  ]

badCases = [
  "foo;",
  "OP_DOESNOTEXIST;",
  "OP_PUSHDATA 0;",
  "OP_PUSHDATA 76;",
  "OP_PUSHDATA1 100 1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111;",
  "OP_PUSHDATA1 1 2342"
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
    runTest code = TestCase $
      case run_parser "<<test>>" code of
        Left err -> putStrLn $ infoString code err
        Right _ -> assertFailure "Parser should have failed"

infoString :: String -> String -> String
infoString code err =
  "'" ++ code ++ "' -> " ++ (last $ lines $ err)

