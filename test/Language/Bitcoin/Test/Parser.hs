module Language.Bitcoin.Test.Parser
(
  tests
) where

import Language.Bitcoin.Parser (run_parser')
import Language.Bitcoin.Types
import Test.HUnit

tests = TestLabel "Parser" $ TestList $ good ++ bad

goodCases = [
    ("OP_FALSE\n", [OP_FALSE])
  , ("OP_FALSE;", [OP_FALSE])
  , (" OP_FALSE;", [OP_FALSE])
  , ("OP_FALSE;OP_TRUE;", [OP_FALSE, OP_TRUE])
  , ("OP_FALSE\nOP_TRUE;", [OP_FALSE, OP_TRUE])
  ]

badCases = [
  "OP_DOESNOTEXIST;"
  ]

good :: [Test]
good = map runTest goodCases
  where
    runTest (code, expected) = TestCase $
      case run_parser' "<<test>>" code of
        Left e -> assertString $ show e
        Right script -> expected @=? script


bad :: [Test]
bad = map runTest badCases
  where
    runTest code = TestCase $
      case run_parser' "<<test>>" code of
        Left _ -> return ()
        Right _ -> assertString "Parser should have failed"
