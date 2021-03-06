module Language.Bitcoin.Types where
-- import {{{1
import qualified Data.ByteString as B
import Data.Int (Int32)
import qualified Data.List as List

-- types {{{1
type Binary = B.ByteString
type Script = [Command]
type Program = [Opcode]
type Code = String

type Stack = [B.ByteString]

data Keypair = Keypair {
    keyPublic :: B.ByteString
  , keyPrivate :: B.ByteString
  } deriving (Show, Eq)

type Keyring = [Keypair]

data Machine = Machine {
    mchProgram :: Program
  , mchKeyring :: Keyring
  , mchStack :: Stack
  , mchAltStack :: Stack
  }

data ResultCode =
    Success
  | Failure String
  | Error String


data Result = Result ResultCode Machine

data Opcode = -- {{{1
-- constants {{{2
    OP_FALSE
  | OP_TRUE
  | OP_1NEGATE
  | OP_0
  | OP_1
  | OP_2
  | OP_3
  | OP_4
  | OP_5
  | OP_6
  | OP_7
  | OP_8
  | OP_9
  | OP_10
  | OP_11
  | OP_12
  | OP_13
  | OP_14
  | OP_15
  | OP_16
-- flow control {{{2
  | OP_NOP
  | OP_IF
  | OP_NOTIF
  | OP_ELSE
  | OP_ENDIF
  | OP_VERIFY
  | OP_RETURN
-- stack {{{2
  | OP_TOALTSTACK
  | OP_FROMALTSTACK 
  | OP_IFDUP
  | OP_DEPTH
  | OP_DROP 
  | OP_DUP
  | OP_NIP
  | OP_OVER 
  | OP_PICK 
  | OP_ROLL 
  | OP_ROT
  | OP_SWAP 
  | OP_TUCK 
  | OP_2DROP
  | OP_2DUP 
  | OP_3DUP 
  | OP_2OVER
  | OP_2ROT 
  | OP_2SWAP
-- splice {{{2
  | OP_CAT
  | OP_SUBSTR
  | OP_LEFT 
  | OP_RIGHT
  | OP_SIZE
-- bitwise logic {{{2
  | OP_INVERT
  | OP_AND
  | OP_OR
  | OP_XOR
  | OP_EQUAL
  | OP_EQUALVERIFY
-- arithmetic {{{2
  | OP_1ADD 
  | OP_1SUB 
  | OP_2MUL 
  | OP_2DIV 
  | OP_NEGATE
  | OP_ABS
  | OP_NOT
  | OP_0NOTEQUAL
  | OP_ADD
  | OP_SUB
  | OP_MUL
  | OP_DIV
  | OP_MOD
  | OP_LSHIFT
  | OP_RSHIFT
  | OP_BOOLAND
  | OP_BOOLOR
  | OP_NUMEQUAL 
  | OP_NUMEQUALVERIFY
  | OP_NUMNOTEQUAL
  | OP_LESSTHAN 
  | OP_GREATERTHAN
  | OP_LESSTHANOREQUAL
  | OP_GREATERTHANOREQUAL
  | OP_MIN
  | OP_MAX
  | OP_WITHIN
-- crypto {{{2
  | OP_RIPEMD160
  | OP_SHA1 
  | OP_SHA256
  | OP_HASH160
  | OP_HASH256
  | OP_CODESEPARATOR
  | OP_CHECKSIG 
  | OP_CHECKSIGVERIFY
  | OP_CHECKMULTISIG
  | OP_CHECKMULTISIGVERIFY
-- pseudo {{{2
  | OP_PUBKEYHASH
  | OP_PUBKEY
  | OP_INVALIDOPCODE
-- reserved {{{2
  | OP_RESERVED 
  | OP_VER
  | OP_VERIF
  | OP_VERNOTIF 
  | OP_RESERVED1
  | OP_RESERVED2
  | OP_NOP1
  | OP_NOP2
  | OP_NOP3
  | OP_NOP4
  | OP_NOP5
  | OP_NOP6
  | OP_NOP7
  | OP_NOP8
  | OP_NOP9
  | OP_NOP10
-- data {{{2
  | OP_PUSHDATA PushDataType B.ByteString
  deriving (Show, Eq, Read)

data PushDataType =
    Direct
  | OneByte
  | TwoBytes
  | FourBytes
  deriving (Show, Eq, Read)

data Command = -- {{{1
    CmdOpcode Opcode
  | DATA B.ByteString
  | KEY Int32
  | SIG Int32
  deriving (Show, Eq)

