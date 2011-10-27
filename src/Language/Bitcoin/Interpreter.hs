{-# OPTIONS_GHC -fno-warn-incomplete-patterns #-}
module Language.Bitcoin.Interpreter
(
	run_interpreter, run_interpreter'
) where

import Control.Arrow ((***), Arrow)
import Language.Bitcoin.Types
import Language.Bitcoin.Utils (b2i, i2b)
import Language.Bitcoin.Text (print_result)
import qualified Data.ByteString.Lazy as B

run_interpreter :: Program -> Keyring -> Either String Result
run_interpreter program keyring =
  case run_interpreter' (Machine program keyring [] [])  of
    result@(Result (Error _) _) -> Left $ print_result result
    result -> Right result

run_interpreter' :: Machine -> Result
run_interpreter' machine@(Machine [] _ stack _) =
  if topIsTrue stack
    then Result Success machine
    else Result (Failure "top stack value is not True") machine

run_interpreter' machine@(Machine (OP_TOALTSTACK:_) _ [] _) =
  Result (Error "OP_TOALTSTACK failed because the stack is empty") machine
run_interpreter' (Machine (OP_TOALTSTACK:rest) keyring (top:rest') altStack) =
  run_interpreter' (Machine rest keyring rest' (top:altStack))

run_interpreter' machine@(Machine (OP_FROMALTSTACK:_) _ _ []) =
  Result (Error "OP_FROMALTSTACK failed because the alt stack is empty") machine
run_interpreter' (Machine (OP_FROMALTSTACK:rest) keyring stack (top:rest')) =
  run_interpreter' (Machine rest keyring (top:stack) rest')  

run_interpreter' machine@(Machine (op:rest) keyring stack altStack) =
	case simpleOp op stack of
		Left code -> Result code machine
		Right (stack') -> run_interpreter' (Machine rest keyring stack' altStack)

topIsTrue :: [B.ByteString] -> Bool
topIsTrue (x:_) =
  case b2i x of
    Left _ -> False
    Right value -> value == 1
topIsTrue _ = False

simpleOp :: Opcode -> Stack -> Either ResultCode Stack
simpleOp OP_FALSE stack = Right $ i2b  0 : stack
simpleOp OP_TRUE  stack = Right $ i2b  1 : stack
simpleOp OP_0     stack = Right $ i2b  0 : stack
simpleOp OP_1     stack = Right $ i2b  1 : stack
simpleOp OP_2     stack = Right $ i2b  2 : stack
simpleOp OP_3     stack = Right $ i2b  3 : stack
simpleOp OP_4     stack = Right $ i2b  4 : stack
simpleOp OP_5     stack = Right $ i2b  5 : stack
simpleOp OP_6     stack = Right $ i2b  6 : stack
simpleOp OP_7     stack = Right $ i2b  7 : stack
simpleOp OP_8     stack = Right $ i2b  8 : stack
simpleOp OP_9     stack = Right $ i2b  9 : stack
simpleOp OP_10    stack = Right $ i2b 10 : stack
simpleOp OP_11    stack = Right $ i2b 11 : stack
simpleOp OP_12    stack = Right $ i2b 12 : stack
simpleOp OP_13    stack = Right $ i2b 13 : stack
simpleOp OP_14    stack = Right $ i2b 14 : stack
simpleOp OP_15    stack = Right $ i2b 15 : stack
simpleOp OP_16    stack = Right $ i2b 16 : stack

simpleOp OP_NOP   stack = Right stack
simpleOp OP_RETURN _ = Left $ Failure $ "script failes as requested by OP_RETURN."

simpleOp OP_VERIFY stack =
  if topIsTrue stack
    then Right $ tail stack
    else Left $ Failure $ "OP_VERIFY failed because top stack value is not True."

simpleOp OP_IFDUP _ = Left $ Error "OP_IFDUP is broken, so it's not supported."
simpleOp OP_DEPTH stack = Right $ ((i2b . fromIntegral . length) stack) : stack
simpleOp OP_DROP stack = stackOp stack 1 (\(_:xs) -> Right xs)
simpleOp OP_DUP stack = stackOp stack 1 (\(x:xs) -> Right $ x:x:xs)
simpleOp OP_NIP stack = stackOp stack 2 (\(x:_:xs) -> Right $ x:xs)
simpleOp OP_OVER stack = stackOp stack 2 (\(x1:x2:xs) -> Right $ x2:x1:x2:xs)

simpleOp OP_PICK stack = stackOp stack 1 (\(n:xs) -> opPick (b2i n) xs)
  where
    opPick (Left e) _ = Left $ Error e
    opPick (Right n) xs =
      let n' = fromIntegral n in
      stackOp xs n' (\xs' -> Right $ (head (take n' xs')) : xs')

simpleOp OP_ROLL stack = stackOp stack 1 (\(n:xs) -> opRoll (b2i n) xs)
  where
    opRoll (Left e) _ = Left $ Error e
    opRoll (Right n) xs =
      let n' = fromIntegral n in
      stackOp xs n' (\xs' -> Right $ (take (n'-1) xs') ++ (drop n' xs'))

simpleOp OP_ROT stack = stackOp stack 3 (\(x1:x2:x3:xs) -> Right $ x3:x1:x2:xs)
simpleOp OP_SWAP stack = stackOp stack 2 (\(x1:x2:xs) -> Right $ x2:x1:xs)
simpleOp OP_TUCK stack = stackOp stack 2 (\(x1:x2:xs) -> Right $ x1:x2:x1:xs)
simpleOp OP_2DROP stack = stackOp stack 2 (\(_:_:xs) -> Right xs)
simpleOp OP_2DUP stack = stackOp stack 2 (\(x1:x2:xs) -> Right $ x1:x2:x1:x2:xs)
simpleOp OP_3DUP stack = stackOp stack 3 (\(x1:x2:x3:xs) -> Right $ x1:x2:x3:x1:x2:x3:xs)
simpleOp OP_2OVER stack = stackOp stack 4 (\(x1:x2:x3:x4:xs) -> Right $ x3:x4:x1:x2:x3:x4:xs)
simpleOp OP_2ROT stack = stackOp stack 6 (\(x1:x2:x3:x4:x5:x6:xs) -> Right $ x5:x6:x1:x2:x3:x4:xs)
simpleOp OP_2SWAP stack = stackOp stack 4 (\(x1:x2:x3:x4:xs) -> Right $ x3:x4:x1:x2:xs)


simpleOp OP_CAT stack = stackOp stack 2 (\(x1:x2:xs) -> Right $ (B.append x1 x2) : xs)

simpleOp OP_SUBSTR stack = stackOp stack 3 (\(size:begin:bytes:xs) -> opSubstr (b2i size) (b2i begin) bytes xs)
  where
    opSubstr (Left e) _ _ _ = Left $ Error e
    opSubstr _ (Left e) _ _ = Left $ Error e
    opSubstr (Right size) (Right begin) bytes xs =
      let (size', begin') = tmap fromIntegral (size, begin) in
      if B.length bytes < begin' + size'
        then Left $ Error "OP_SUBSTR goes beyond the end of the string"
        else Right $ (B.take size' $ B.drop begin' bytes) : xs

simpleOp OP_LEFT stack = stackOp stack 2 (\(size:bytes:xs) -> opLeft (b2i size) bytes xs)
  where
    opLeft (Left e) _ _ = Left $ Error e
    opLeft (Right size) bytes xs =
      let size' = fromIntegral size in
      if B.length bytes < size'
        then Left $ Error "OP_LEFT goes beyond the end of the string"
        else Right $ (B.take size' bytes) : xs

simpleOp OP_RIGHT stack = stackOp stack 2 (\(size:bytes:xs) -> opRight (b2i size) bytes xs)
  where
    opRight (Left e) _ _ = Left $ Error e
    opRight (Right size) bytes xs =
      let size' = fromIntegral size in
      if B.length bytes < size'
        then Left $ Error "OP_RIGHT goes beyond the end of the string"
        else Right $ (B.drop size' bytes) : xs

simpleOp OP_SIZE stack = stackOp stack 1 (\(bytes:xs) -> Right $ ((i2b . fromIntegral . B.length) bytes) : xs)
    

simpleOp OP_PUBKEYHASH _ = pseudoOp OP_PUBKEYHASH
simpleOp OP_PUBKEY _ = pseudoOp OP_PUBKEY
simpleOp OP_INVALIDOPCODE _ = pseudoOp OP_INVALIDOPCODE

simpleOp OP_RESERVED _  = reservedOp OP_RESERVED 
simpleOp OP_VER _       = reservedOp OP_VER
simpleOp OP_VERIF _     = reservedOp OP_VERIF
simpleOp OP_VERNOTIF  _ = reservedOp OP_VERNOTIF 
simpleOp OP_RESERVED1 _ = reservedOp OP_RESERVED1
simpleOp OP_RESERVED2 _ = reservedOp OP_RESERVED2
simpleOp OP_NOP1 _      = reservedOp OP_NOP1
simpleOp OP_NOP2 _      = reservedOp OP_NOP2
simpleOp OP_NOP3 _      = reservedOp OP_NOP3
simpleOp OP_NOP4 _      = reservedOp OP_NOP4
simpleOp OP_NOP5 _      = reservedOp OP_NOP5
simpleOp OP_NOP6 _      = reservedOp OP_NOP6
simpleOp OP_NOP7 _      = reservedOp OP_NOP7
simpleOp OP_NOP8 _      = reservedOp OP_NOP8
simpleOp OP_NOP9 _      = reservedOp OP_NOP9
simpleOp OP_NOP10 _     = reservedOp OP_NOP10


simpleOp (OP_PUSHDATA _ bytes) stack = Right $ bytes : stack
simpleOp op _ = Left $ Error $ "sorry, opcode " ++ show op ++ " is not implemented yet."


pseudoOp :: Opcode -> Either ResultCode a
pseudoOp x = Left $ Error $ show x ++ " is a pseudo opcode. It can not be executed."

reservedOp :: Opcode -> Either ResultCode a
reservedOp x = Left $ Error $ show x ++ " is a reserved opcode. It may not be used in scripts."

stackOp :: Stack -> Int -> (Stack -> Either ResultCode Stack) -> Either ResultCode Stack
stackOp stack count operation =
  if length stack < count
    then Left $ Error $ "operation failed because there are less than " ++ show count ++ " elements on the stack"
    else operation stack

tmap :: Arrow a => a b c -> a (b, b) (c, c)
tmap f = f *** f
