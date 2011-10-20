module Language.Bitcoin.Machine where

import Language.Bitcoin.Opcodes (Opcode)
import Data.ByteString

data Word =
    WrdData ByteString
  | WrdNum Int
	deriving (Show, Eq)
  
type Stack = [Word]

data Command =
    CmdOpcode Opcode
  | CmdData ByteString
	deriving (Show, Eq)
  
type Script = [Command]

data Machine = Machine Script Stack deriving (Show)
