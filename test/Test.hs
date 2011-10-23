import Test.HUnit (Test(TestList), runTestText, putTextToHandle, Counts(errors, failures))
import System.IO (stderr)
import System.Exit (exitWith, ExitCode(ExitFailure))

import qualified Language.Bitcoin.Test.Interpreter as A
import qualified Language.Bitcoin.Test.Parser as B
import qualified Language.Bitcoin.Test.Utils as C

tests = TestList [A.tests, B.tests, C.tests]

main = do
  (count, _ ) <- runTestText (putTextToHandle stderr False) tests
  if errors count > 0 || failures count > 0
    then
      exitWith $ ExitFailure 1
    else
      return ()
