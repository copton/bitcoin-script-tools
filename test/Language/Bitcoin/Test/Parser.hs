module Language.Bitcoin.Test.Parser
(
  tests
) where

import Data.Binary (encode)
import Language.Bitcoin.Parser (run_parser)
import Language.Bitcoin.Types
import Test.HUnit
import qualified Data.ByteString.Lazy as B

tests = TestLabel "Parser" $ TestList $ good ++ bad

goodCases = [
    ("OP_FALSE\n", [CmdOpcode OP_FALSE])
  , ("OP_FALSE;", [CmdOpcode OP_FALSE])
  , (" OP_FALSE;", [CmdOpcode OP_FALSE])
  , ("OP_FALSE;OP_TRUE;", [CmdOpcode OP_FALSE, CmdOpcode OP_TRUE])
  , ("OP_FALSE\nOP_TRUE;", [CmdOpcode OP_FALSE, CmdOpcode OP_TRUE])
  , ("DATA 23;", [DATA 0x23])
  ]

badCases = [
  "OP_DOESNOTEXIST;", "foo\n"
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
        Left _ -> return ()
        Right _ -> assertString "Parser should have failed"
