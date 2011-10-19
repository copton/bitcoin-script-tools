module Language.Bitcoin.Opcodes 
(
  Opcode(..)
) where

import Data.Maybe (fromJust)
import Data.List (find)

data Opcode =
-- constants
    OP_0 | OP_FALSE
  | LENGTH Int
  | OP_PUSHDATA1
  | OP_PUSHDATA2
  | OP_PUSHDATA4
  | OP_1NEGATE
  | OP_1 | OP_TRUE
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
-- flow control
  | OP_NOP
  | OP_IF
  | OP_NOTIF
  | OP_ELSE
  | OP_ENDIF
  | OP_VERIFY
  | OP_RETURN
-- stack
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
-- splice
  | OP_CAT
  | OP_SUBSTR
  | OP_LEFT 
  | OP_RIGHT
  | OP_SIZE
-- bitwise logic
  | OP_INVERT
  | OP_AND
  | OP_OR
  | OP_XOR
  | OP_EQUAL
  | OP_EQUALVERIFY
-- arithmetic
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
-- crypto
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
-- pseudo
  | OP_PUBKEYHASH
  | OP_PUBKEY
  | OP_INVALIDOPCODE
-- reserved
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
  deriving (Show, Read, Eq)

opcodes :: [(Opcode, Int)]
opcodes = [
    (OP_FALSE, 0)
  , (OP_0, 0)
  , (LENGTH 1, 1)
  , (OP_PUSHDATA1, 76)
  , (OP_PUSHDATA2, 77)
  , (OP_PUSHDATA4, 78)
  , (OP_1NEGATE, 79)
  , (OP_TRUE, 81)
  , (OP_1, 81)
  , (OP_2, 82)
  , (OP_3, 83)
  , (OP_4, 84)
  , (OP_5, 85)
  , (OP_6, 86)
  , (OP_7, 87)
  , (OP_8, 88)
  , (OP_9, 89)
  , (OP_10, 90)
  , (OP_11, 91)
  , (OP_12, 92)
  , (OP_13, 93)
  , (OP_14, 94)
  , (OP_15, 95)
  , (OP_16, 96)
  , (OP_NOP, 97)
  , (OP_IF, 99)
  , (OP_NOTIF, 100)
  , (OP_ELSE, 103)
  , (OP_ENDIF, 104)
  , (OP_VERIFY, 105)
  , (OP_RETURN, 106)
  , (OP_TOALTSTACK, 107)
  , (OP_FROMALTSTACK, 108)
  , (OP_IFDUP, 115)
  , (OP_DEPTH, 116)
  , (OP_DROP, 117)
  , (OP_DUP, 118)
  , (OP_NIP, 119)
  , (OP_OVER, 120)
  , (OP_PICK, 121)
  , (OP_ROLL, 122)
  , (OP_ROT, 123)
  , (OP_SWAP, 124)
  , (OP_TUCK, 125)
  , (OP_2DROP, 109)
  , (OP_2DUP, 110)
  , (OP_3DUP, 111)
  , (OP_2OVER, 112)
  , (OP_2ROT, 113)
  , (OP_2SWAP, 114)
  , (OP_CAT, 126)
  , (OP_SUBSTR, 127)
  , (OP_LEFT, 128)
  , (OP_RIGHT, 129)
  , (OP_SIZE, 130)
  , (OP_INVERT, 131)
  , (OP_AND, 132)
  , (OP_OR, 133)
  , (OP_XOR, 134)
  , (OP_EQUAL, 135)
  , (OP_EQUALVERIFY, 136)
  , (OP_1ADD, 139)
  , (OP_1SUB, 140)
  , (OP_2MUL, 141)
  , (OP_2DIV, 142)
  , (OP_NEGATE, 143)
  , (OP_ABS, 144)
  , (OP_NOT, 145)
  , (OP_0NOTEQUAL, 146)
  , (OP_ADD, 147)
  , (OP_SUB, 148)
  , (OP_MUL, 149)
  , (OP_DIV, 150)
  , (OP_MOD, 151)
  , (OP_LSHIFT, 152)
  , (OP_RSHIFT, 153)
  , (OP_BOOLAND, 154)
  , (OP_BOOLOR, 155)
  , (OP_NUMEQUAL, 156)
  , (OP_NUMEQUALVERIFY, 157)
  , (OP_NUMNOTEQUAL, 158)
  , (OP_LESSTHAN, 159)
  , (OP_GREATERTHAN, 160)
  , (OP_LESSTHANOREQUAL, 161)
  , (OP_GREATERTHANOREQUAL, 162)
  , (OP_MIN, 163)
  , (OP_MAX, 164)
  , (OP_WITHIN, 165)
  , (OP_RIPEMD160, 166)
  , (OP_SHA1, 167)
  , (OP_SHA256, 168)
  , (OP_HASH160, 169)
  , (OP_HASH256, 170)
  , (OP_CODESEPARATOR, 171)
  , (OP_CHECKSIG, 172)
  , (OP_CHECKSIGVERIFY, 173)
  , (OP_CHECKMULTISIG, 174)
  , (OP_CHECKMULTISIGVERIFY, 175)
  , (OP_PUBKEYHASH, 253)
  , (OP_PUBKEY, 254)
  , (OP_INVALIDOPCODE, 255)
  , (OP_RESERVED, 80)
  , (OP_VER, 98)
  , (OP_VERIF, 101)
  , (OP_VERNOTIF, 102)
  , (OP_RESERVED1, 137)
  , (OP_RESERVED2, 138)
  , (OP_NOP1, 176)
  , (OP_NOP2, 177)
  , (OP_NOP3, 178)
  , (OP_NOP4, 179)
  , (OP_NOP5, 180)
  , (OP_NOP6, 181)
  , (OP_NOP7, 182)
  , (OP_NOP8, 183)
  , (OP_NOP9, 184)
  , (OP_NOP10, 185)
  ]
  
instance Enum Opcode where
  fromEnum (LENGTH length)
    | length >= 1 && length < 76 = length
    | otherwise = error "illegal LENGTH opcode"
  fromEnum opcode = fromJust $ lookup opcode opcodes
  
  toEnum number
    | number >= 1 && number < 76 = LENGTH number
    | otherwise = fst $ fromJust $ find ((==number) . snd) opcodes
  
