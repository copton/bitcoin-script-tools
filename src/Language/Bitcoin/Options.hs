module Language.Bitcoin.Options
-- export {{{1
(
	Options(..), options, defaultOptions, Action(..)
) where

-- import {{{1
import Data.List (intersperse)
import System.Console.GetOpt

-- types {{{1
data Action =
	  Assembler
  | Dissasembler
  | Siulator

data Options = Options { 
		optInput :: String
	, optOutput :: String
	, optAssembler :: Bool
	, optDisassembler :: Bool
	, optSimulator :: Bool
	, optHelp :: Bool
} deriving Show

defaultOptions :: Options
defaultOptions = Options { 
	  optInput = "-"
	, optOutput = "-"
	, optSimulator = False
	, optAssembler = False
	, optDisassembler = False
	, optHelp = False
}

-- options :: String -> [String] -> Either String Options {{{1
options :: String -> [String] -> Either String Options
options prg argv =
	let use = usage prg in
	case getOpt Permute available_options argv of
		(o,[],[]) -> 
			let opts = foldl (flip id) defaultOptions o in
			if (optHelp opts) then
					Left $ help use
			else
				case checkOptions opts of
					Nothing -> Right opts
					Just s -> Left $ err use s
		(_,n,[]) -> Left $ err use ("unknown options '" ++ unwords n ++ "'")
		(_,_,es) -> Left $ err use $ concat $ intersperse "\n" es

help :: String -> String
help use = "Printing usage:\n" ++ use

err :: String -> String -> String
err use msg = "Error in command line options\n" ++ msg ++ "\n" ++ use

usage :: String -> String
usage prg = usageInfo ("Usage: " ++ prg ++ " OPTIONS") available_options

available_options :: [OptDescr (Options -> Options)]
available_options = [
	  Option ['i'] ["input"] (ReqArg (\x opts -> opts {optInput = x}) "input") "input file (default '-')"
	, Option ['o'] ["output"] (ReqArg (\x opts -> opts {optOutput = x}) "output") "output file (default '-')"
	, Option ['s'] ["assembler"] (NoArg (\opts -> opts {optSimulator = True})) "run the simulator"
	, Option ['a'] ["assembler"] (NoArg (\opts -> opts {optAssembler = True})) "run the assembler"
	, Option ['d'] ["disassembler"] (NoArg (\opts -> opts {optDisassembler = True})) "run the disassembler"
	, Option ['h'] ["help"] (NoArg (\opts -> opts {optHelp = True}))  "print help and quit"
	]


b2n :: Bool -> Int
b2n True = 1
b2n False = 0

checkOptions :: Options -> Maybe String
checkOptions opts
	| b2n (optSimulator opts) + b2n (optAssembler opts) + b2n (optDisassembler opts) /= 1 = Just "please choose exactly one action to run"
	| otherwise = Nothing
