module Language.Bitcoin.Main where

import qualified Data.ByteString.Char8 as B
import Language.Bitcoin.Options
import Language.Bitcoin.Types (ResultCode(Error), Result(Result))
import Language.Bitcoin.Simulator (run_simulator)
import Language.Bitcoin.Assembler (run_assembler, run_disassembler)
import Language.Bitcoin.Parser (run_parser)
import System.Environment (getArgs, getProgName)
import System.Exit (exitWith, ExitCode(ExitFailure))
import System.IO (stderr, hPutStrLn)

main :: IO ()
main = do
	prg <- getProgName
	args <- getArgs
	opts <- exitOnError $ options prg args
	input <- B.readFile $ optInput opts
	output <- exitOnError $ runAction opts input
	if (optOutput opts /= optOutput defaultOptions)
		then B.writeFile (optOutput opts) output
		else putStrLn $ B.unpack output

runAction :: Options -> B.ByteString -> Either String B.ByteString
runAction opts input
	| optAssembler opts = Right $ assembler input
	| optDisassembler opts = Right $ disassembler input
	| optSimulator opts = simulator opts input
	| otherwise = error "internal error"

assembler :: B.ByteString -> B.ByteString
assembler = run_assembler

disassembler :: B.ByteString -> B.ByteString
disassembler = run_disassembler

simulator :: Options -> B.ByteString -> Either String B.ByteString
simulator opts code =
	case run_parser (optInput opts) code of
		Left parseError -> Left $ show parseError
		Right script ->
			case run_simulator script of
				result@(Result (Error _) _) -> Left $ show result
				result -> Right $ B.pack (show result)

exitOnError :: Either String a -> IO a
exitOnError (Left e) = hPutStrLn stderr e >> exitWith (ExitFailure 1)
exitOnError (Right x) = return x
