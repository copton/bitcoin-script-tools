import Control.Monad (forM_)
import Test.HUnit (Test(TestList), runTestText, putTextToHandle, Counts(errors, failures))
import Test.QuickCheck (Result(Success))
import System.IO (stdout, hFlush)
import System.Exit (exitWith, ExitCode(ExitFailure))

import qualified Language.Bitcoin.Test.Interpreter as U1
import qualified Language.Bitcoin.Test.Parser as U2
import qualified Language.Bitcoin.Test.Preprocessor as U3

import qualified Language.Bitcoin.Test.Assembler as Q1

unitTests :: IO ()
unitTests = do
  let tests = TestList [U1.tests, U2.tests, U3.tests]
  (count, _ ) <- runTestText (putTextToHandle stdout False) tests
  if errors count > 0 || failures count > 0
    then
      exitWith $ ExitFailure 1
    else
      return ()

smokeTests :: IO ()
smokeTests = forM_ [Q1.test] $ \test -> do
  result <- test
  case result of
    (Success _ _ _) -> return ()
    _ -> exitWith $ ExitFailure 1

main = do
  putStrLn ">>> Running Unit Tests <<<"
  unitTests
  putStrLn ">>> Running Smoke Tests <<<"
  smokeTests
