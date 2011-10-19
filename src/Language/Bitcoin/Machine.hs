module Language.Bitcoin.Machine where

import Language.Bitcoin.Opcodes (Opcode)
import Data.ByteString

data Word =
    WrdData ByteString
  | WrdNum Int
  
type Stack = [Word]

data Command =
    CmdOpcode Opcode
  | CmdData ByteString
  
type Script = [Command]

data Machine = Machine Stack Script
