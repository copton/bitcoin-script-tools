module Language.Bitcoin.Main where

import qualified Data.ByteString as B
import Language.Bitcoin.Options
import Language.Bitcoin.Interpreter (run_interpreter)
import Language.Bitcoin.Preprocessor (run_preprocessor)
import Language.Bitcoin.Assembler (run_assembler, run_disassembler)
import Language.Bitcoin.Parser (run_parser)
import Language.Bitcoin.Printer (run_printer)
import Language.Bitcoin.Text (print_result)
import System.Environment (getArgs, getProgName)
import System.Exit (exitWith, ExitCode(ExitFailure))
import System.IO (stderr, hPutStrLn, hGetContents, hPutStr, hFlush, Handle, stdin, stdout, openFile, IOMode(ReadMode, WriteMode))


main :: IO ()
main = do
  prg <- getProgName
  args <- getArgs
  opts <- exitOnError $ options prg args
  runAction opts

runAction :: Options -> IO ()
runAction opts
  | optAssembler opts = assembler opts
  | optDisassembler opts = disassembler opts
  | optSimulator opts = interpreter opts
  | otherwise = error "internal error"

assembler :: Options -> IO ()
assembler opts = do
  (hIn, name) <- fileIn $ optInput opts
  script <- hGetContents hIn
  result <- exitOnError $
        run_parser name script
    >>= run_assembler . fst . run_preprocessor
  hOut <- fileOut $ optOutput opts
  B.hPutStr hOut result
  hFlush hOut

disassembler :: Options -> IO ()
disassembler opts = do
  (hIn, _) <- fileIn $ optInput opts
  binary <- B.hGetContents hIn
  result <- exitOnError $
        run_disassembler binary
    >>= return . run_printer
  hOut <- fileOut $ optOutput opts
  hPutStr hOut result
  hFlush hOut

interpreter :: Options -> IO ()
interpreter opts = do
  (hIn, name) <- fileIn $ optInput opts
  script <- hGetContents hIn
  result <- exitOnError $
        run_parser name script
    >>= (uncurry run_interpreter) . run_preprocessor
    >>= return . print_result
  hOut <- fileOut $ optOutput opts
  hPutStr hOut result
  hFlush hOut

fileIn :: String -> IO (Handle, String)
fileIn name
  | name == "-" = return (stdin, "<<stdin>>")
  | otherwise = do
    handle <- openFile name ReadMode
    return (handle, name)

fileOut :: String -> IO Handle
fileOut name
  | name == "-" = return stdout
  | otherwise = openFile name WriteMode

exitOnError :: Either String a -> IO a
exitOnError (Left e) = hPutStrLn stderr e >> exitWith (ExitFailure 1)
exitOnError (Right x) = return x
