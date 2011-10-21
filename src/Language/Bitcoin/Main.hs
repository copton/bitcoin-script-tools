module Language.Bitcoin.Main where

import Language.Bitcoin.Options
import Language.Bitcoin.Simulator (run_simulator)
import Language.Bitcoin.Preprocessor (run_preprocessor)
--import Language.Bitcoin.Assembler (run_assembler, run_disassembler)
import Language.Bitcoin.Parser (run_parser)
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
  | optSimulator opts = simulator opts
  | otherwise = error "internal error"

assembler :: Options -> IO ()
assembler = undefined

disassembler :: Options -> IO ()
disassembler = undefined

simulator :: Options -> IO ()
simulator opts = do
  (hIn, name) <- fileIn $ optInput opts
  code <- hGetContents hIn
  result <- exitOnError $
        run_parser name code 
    >>= run_preprocessor
    >>= run_simulator
    >>= return . show
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
