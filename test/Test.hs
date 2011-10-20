import Test.HUnit (Test(TestList), runTestText, putTextToHandle, Counts(errors, failures))
import System.IO (stderr)
import System.Exit (exitWith, ExitCode(ExitFailure))

import qualified Language.Bitcoin.Test.Opcodes as A

tests = TestList [A.tests]

main = do
  (count, _ ) <- runTestText (putTextToHandle stderr False) tests
  if errors count > 0 || failures count > 0
    then
      exitWith $ ExitFailure 1
    else
      return ()
