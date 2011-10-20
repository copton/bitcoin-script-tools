module Language.Bitcoin.Machine where

import Language.Bitcoin.Opcodes (Opcode)
import Data.Word

type Data = [Word8]
type Stack = [Data]

data Command =
    CmdOpcode Opcode
  | CmdData Data
	deriving (Show, Eq)
  
type Script = [Command]

data Machine = Machine Script Stack Stack deriving (Show)

data ResultCode =
	  Success
  | Failure String
  | Error String

instance Show ResultCode where
	show Success = "Bitcoin script completed successfully."
	show (Failure what) = "Bitcoin script failed: " ++ what
	show (Error what) = "Bitcoin script is illegal: " ++ what

data Result = Result Machine ResultCode deriving (Show)
