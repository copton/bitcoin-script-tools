import Test.HUnit (Test(TestList), runTestText, putTextToHandle, Counts(errors, failures))
import System.IO (stderr)
import System.Exit (exitWith, ExitCode(ExitFailure))

import qualified Language.Bitcoin.Test.Interpreter as D
import qualified Language.Bitcoin.Test.Parser as B
import qualified Language.Bitcoin.Test.Preprocessor as C
import qualified Language.Bitcoin.Test.Utils as A

tests = TestList [A.tests, B.tests, C.tests, D.tests]

main = do
  (count, _ ) <- runTestText (putTextToHandle stderr False) tests
  if errors count > 0 || failures count > 0
    then
      exitWith $ ExitFailure 1
    else
      return ()
