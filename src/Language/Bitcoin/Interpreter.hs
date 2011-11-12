{-# OPTIONS_GHC -fno-warn-incomplete-patterns #-}
module Language.Bitcoin.Interpreter
-- export {{{1
(
	run_interpreter, exec
) where

-- import {{{1
import Data.Bits (complement, (.|.), (.&.), xor)
import Data.Word (Word8)
import Language.Bitcoin.Types
import Language.Bitcoin.Numbers
import Language.Bitcoin.Text (print_result)
import qualified Data.ByteString as B
import qualified Data.List as List

run_interpreter :: Program -> Keyring -> Either String Result -- {{{1
run_interpreter program keyring =
  case exec (Machine program keyring [] [])  of
    result@(Result (Error _) _) -> Left $ print_result result
    result -> Right result

exec :: Machine -> Result
exec machine@(Machine [] _ stack _) =
  if topIsTrue stack
    then Result Success machine
    else Result (Failure "top stack value is not True") machine

-- alt stack {{{2
exec machine@(Machine (OP_TOALTSTACK:_) _ [] _) =
  Result (Error "OP_TOALTSTACK failed because the stack is empty") machine

exec (Machine (OP_TOALTSTACK:rest) keyring (top:rest') altStack) =
  exec (Machine rest keyring rest' (top:altStack))

exec machine@(Machine (OP_FROMALTSTACK:_) _ _ []) =
  Result (Error "OP_FROMALTSTACK failed because the alt stack is empty") machine

exec (Machine (OP_FROMALTSTACK:rest) keyring stack (top:rest')) =
  exec (Machine rest keyring (top:stack) rest')  

-- verify {{{2
exec machine@(Machine (OP_EQUALVERIFY:xs) _ _ _) =
  exec $ machine { mchProgram = OP_EQUAL:OP_VERIFY:xs }

exec machine@(Machine (OP_NUMEQUALVERIFY:xs) _ _ _) =
  exec $ machine { mchProgram = OP_NUMEQUAL:OP_VERIFY:xs }

exec machine@(Machine (OP_CHECKSIGVERIFY:xs) _ _ _) =
  exec $ machine { mchProgram = OP_CHECKSIG:OP_VERIFY:xs }

exec machine@(Machine (OP_CHECKMULTISIGVERIFY:xs) _ _ _) =
  exec $ machine { mchProgram = OP_CHECKMULTISIG:OP_VERIFY:xs }

-- flow control {{{2
exec machine@(Machine (OP_IF:_) _ _ _) = execIfBlock id machine
exec machine@(Machine (OP_NOTIF:_) _ _ _) = execIfBlock not machine
exec machine@(Machine (OP_ELSE:_) _ _ _) = Result (Error "OP_ELSE without if block") machine
exec machine@(Machine (OP_ENDIF:_) _ _ _) = Result (Error "OP_ENDIF withtout if block") machine
exec machine@(Machine (OP_NOP:xs) _ _ _) = exec (machine { mchProgram = xs })
exec machine@(Machine (OP_RETURN:_) _ _ _) = Result (Failure "script failes as requested by OP_RETURN.") machine
exec machine@(Machine (OP_VERIFY:xs) _ stack _) =
  if topIsTrue stack
    then exec (machine { mchProgram=xs, mchStack = tail stack } )
    else Result (Failure "OP_VERIFY failed because top stack value is not True.") machine

-- simple ops {{{2
exec machine@(Machine (op:rest) keyring stack altStack) =
	case simpleOp op stack of
		Left code -> Result code machine
		Right (stack') -> exec (Machine rest keyring stack' altStack)

simpleOp :: Opcode -> Stack -> Either ResultCode Stack
-- constants -- {{{3
simpleOp OP_FALSE = pushOp  0
simpleOp OP_TRUE  = pushOp  1
simpleOp OP_0     = pushOp  0
simpleOp OP_1     = pushOp  1
simpleOp OP_2     = pushOp  2 
simpleOp OP_3     = pushOp  3 
simpleOp OP_4     = pushOp  4 
simpleOp OP_5     = pushOp  5 
simpleOp OP_6     = pushOp  6 
simpleOp OP_7     = pushOp  7 
simpleOp OP_8     = pushOp  8 
simpleOp OP_9     = pushOp  9 
simpleOp OP_10    = pushOp 10 
simpleOp OP_11    = pushOp 11 
simpleOp OP_12    = pushOp 12 
simpleOp OP_13    = pushOp 13 
simpleOp OP_14    = pushOp 14 
simpleOp OP_15    = pushOp 15 
simpleOp OP_16    = pushOp 16 

-- stack -- {{{3
simpleOp OP_IFDUP = stackOp 1 (\(x:xs) -> if isTrue x then x:x:xs else x:xs)
simpleOp OP_DEPTH = (\stack -> Right $ (fromIntegral . length) stack : stack)
simpleOp OP_DROP  = stackOp 1 (\(_:xs) -> xs)
simpleOp OP_DUP   = stackOp 1 (\(x:xs) -> x:x:xs)
simpleOp OP_NIP   = stackOp 2 (\(x:_:xs) -> x:xs)
simpleOp OP_OVER  = stackOp 2 (\(x1:x2:xs) -> x2:x1:x2:xs)
simpleOp OP_PICK  = stackOp 1 (\(x:xs) -> (head $ take (fromIntegral x) xs) : xs)
simpleOp OP_ROLL  = stackOp 1 (\(x:xs) -> take (fromIntegral x -1) xs ++ drop (fromIntegral x) xs)
simpleOp OP_ROT   = stackOp 3 (\(x1:x2:x3:xs) -> x3:x1:x2:xs)
simpleOp OP_SWAP  = stackOp 2 (\(x1:x2:xs) -> x2:x1:xs)
simpleOp OP_TUCK  = stackOp 2 (\(x1:x2:xs) -> x1:x2:x1:xs)
simpleOp OP_2DROP = stackOp 2 (\(_:_:xs) -> xs)
simpleOp OP_2DUP  = stackOp 2 (\(x1:x2:xs) -> x1:x2:x1:x2:xs)
simpleOp OP_3DUP  = stackOp 3 (\(x1:x2:x3:xs) -> x1:x2:x3:x1:x2:x3:xs)
simpleOp OP_2OVER = stackOp 4 (\(x1:x2:x3:x4:xs) -> x3:x4:x1:x2:x3:x4:xs)
simpleOp OP_2ROT  = stackOp 6 (\(x1:x2:x3:x4:x5:x6:xs) -> x5:x6:x1:x2:x3:x4:xs)
simpleOp OP_2SWAP = stackOp 4 (\(x1:x2:x3:x4:xs) -> x3:x4:x1:x2:xs)

-- splice -- {{{3
simpleOp OP_CAT = stackOp 2 (\(x1:x2:xs) -> (bin2Bci $ B.append (bci2Bin x1) (bci2Bin x2)) : xs)

simpleOp OP_SUBSTR = stackOp' 3 (\(size:begin:bytes:xs) -> opSubstr (fromIntegral size) (fromIntegral begin) (bci2Bin bytes) xs)
  where
    opSubstr size begin bytes xs =
      if B.length bytes < begin + size
        then Left $ Error "OP_SUBSTR goes beyond the end of the string"
        else Right $ bin2Bci (B.take size $ B.drop begin bytes) : xs

simpleOp OP_LEFT = stackOp' 2 (\(size:bytes:xs) -> opLeft (fromIntegral size) (bci2Bin bytes) xs)
  where
    opLeft size bytes xs =
      if B.length bytes < size
        then Left $ Error "OP_LEFT goes beyond the end of the string"
        else Right $ bin2Bci (B.take size bytes) : xs

simpleOp OP_RIGHT = stackOp' 2 (\(size:bytes:xs) -> opRight (fromIntegral size) (bci2Bin bytes) xs)
  where
    opRight size bytes xs =
      if B.length bytes < size
        then Left $ Error "OP_RIGHT goes beyond the end of the string"
        else Right $ bin2Bci (B.drop size bytes) : xs

simpleOp OP_SIZE   = stackOp 1 (\(bytes:xs) -> (fromIntegral . B.length) (bci2Bin bytes) : xs)

-- Bitwise logic -- {{{3
simpleOp OP_INVERT = stackOp 1 (\(bytes:xs) -> bin2Bci (B.map complement (bci2Bin bytes)) : xs)
simpleOp OP_AND    = binaryBitwiseOp (.&.)
simpleOp OP_OR     = binaryBitwiseOp (.|.)
simpleOp OP_XOR    = binaryBitwiseOp xor
simpleOp OP_EQUAL  = stackOp 2 (\(x1:x2:xs) -> (if x1 == x2 then 1 else 0) : xs)

-- arithmetic -- {{{3
simpleOp OP_1ADD = unaryArithmeticOp (+1)
simpleOp OP_1SUB = unaryArithmeticOp ((-)1)
simpleOp OP_2MUL = unaryArithmeticOp (*2)
simpleOp OP_2DIV = unaryArithmeticOp (quot 2)
simpleOp OP_NEGATE = unaryArithmeticOp (* (-1))
simpleOp OP_ABS = unaryArithmeticOp (\x -> if x >= 0 then x else -x)
simpleOp OP_NOT = unaryArithmeticOp (\x -> if x == 0 then 1 else 0)
simpleOp OP_0NOTEQUAL = unaryArithmeticOp (\x -> if x == 0 then 1 else 0)
simpleOp OP_ADD = binaryArithmeticOp (+)
simpleOp OP_SUB = binaryArithmeticOp (-)
simpleOp OP_MUL = binaryArithmeticOp (*)
simpleOp OP_DIV = binaryArithmeticOp quot
simpleOp OP_MOD = binaryArithmeticOp rem
simpleOp OP_LSHIFT = binaryArithmeticOp (\a b -> a * (2^b))
simpleOp OP_RSHIFT = binaryArithmeticOp (\a b -> a `quot` (2^b))
simpleOp OP_BOOLAND = binaryCondition (\a b -> a /= 0 && b /= 0)
simpleOp OP_BOOLOR = binaryCondition (\a b -> a /= 0 || b /= 0)
simpleOp OP_NUMEQUAL = binaryCondition (==)
simpleOp OP_NUMNOTEQUAL = binaryCondition (/=)
simpleOp OP_LESSTHAN = binaryCondition (<)
simpleOp OP_GREATERTHAN = binaryCondition (>)
simpleOp OP_LESSTHANOREQUAL = binaryCondition (<=)
simpleOp OP_GREATERTHANOREQUAL = binaryCondition (>=)
simpleOp OP_MIN = binaryArithmeticOp min
simpleOp OP_MAX = binaryArithmeticOp max
simpleOp OP_WITHIN = stackOp 3 (\(x1:x2:x3:xs) -> (if x1 >= x2 && x1 < x3 then 1 else 0) : xs)

-- crypto -- {{{3
simpleOp OP_RIPEMD160 = undefined
simpleOp OP_SHA1 = undefined
simpleOp OP_SHA256 = undefined
simpleOp OP_HASH160 = undefined
simpleOp OP_HASH256 = undefined
simpleOp OP_CODESEPARATOR = (\stack -> Right stack)
simpleOp OP_CHECKSIG = undefined
simpleOp OP_CHECKMULTISIG = undefined

-- pseude operations -- {{{3
simpleOp OP_PUBKEYHASH    = pseudoOp OP_PUBKEYHASH
simpleOp OP_PUBKEY        = pseudoOp OP_PUBKEY
simpleOp OP_INVALIDOPCODE = pseudoOp OP_INVALIDOPCODE

-- reserved operations -- {{{3
simpleOp OP_RESERVED  = reservedOp OP_RESERVED 
simpleOp OP_VER       = reservedOp OP_VER
simpleOp OP_VERIF     = reservedOp OP_VERIF
simpleOp OP_VERNOTIF  = reservedOp OP_VERNOTIF 
simpleOp OP_RESERVED1 = reservedOp OP_RESERVED1
simpleOp OP_RESERVED2 = reservedOp OP_RESERVED2
simpleOp OP_NOP1      = reservedOp OP_NOP1
simpleOp OP_NOP2      = reservedOp OP_NOP2
simpleOp OP_NOP3      = reservedOp OP_NOP3
simpleOp OP_NOP4      = reservedOp OP_NOP4
simpleOp OP_NOP5      = reservedOp OP_NOP5
simpleOp OP_NOP6      = reservedOp OP_NOP6
simpleOp OP_NOP7      = reservedOp OP_NOP7
simpleOp OP_NOP8      = reservedOp OP_NOP8
simpleOp OP_NOP9      = reservedOp OP_NOP9
simpleOp OP_NOP10     = reservedOp OP_NOP10

simpleOp (OP_PUSHDATA _ bytes) = pushOp bytes
simpleOp op = (\_ -> Left $ Error $ "sorry, opcode " ++ show op ++ " is not implemented yet.")

-- ops {{{2
pushOp :: BCI -> Stack -> Either ResultCode Stack
pushOp x xs = Right $ x:xs

unaryArithmeticOp :: (BCI -> BCI) -> Stack -> Either ResultCode Stack
unaryArithmeticOp operation = stackOp 1 (\(x:xs) -> (operation x) : xs)

binaryArithmeticOp :: (BCI -> BCI -> BCI) -> Stack -> Either ResultCode Stack
binaryArithmeticOp operation = stackOp 2 (\(x1:x2:xs) -> (operation x1 x2) : xs)

binaryCondition :: (BCI -> BCI -> Bool) -> Stack -> Either ResultCode Stack
binaryCondition condition = binaryArithmeticOp (\a b -> if condition a b then 1 else 0)

binaryBitwiseOp :: (Word8 -> Word8 -> Word8) -> Stack -> Either ResultCode Stack
binaryBitwiseOp byteOp = stackOp 2 (\(x1:x2:xs) ->
  bin2Bci (B.pack $ map (uncurry byteOp) $ zip (B.unpack (bci2Bin x1)) (B.unpack (bci2Bin x2))) : xs)

pseudoOp :: Opcode -> Stack -> Either ResultCode a
pseudoOp x _ = Left $ Error $ show x ++ " is a pseudo opcode. It can not be executed."

reservedOp :: Opcode -> Stack -> Either ResultCode a
reservedOp x _ = Left $ Error $ show x ++ " is a reserved opcode. It may not be used in scripts."

stackOp :: Int -> (Stack -> Stack) -> Stack -> Either ResultCode Stack
stackOp count operation stack = stackOp' count (\stack' -> Right $ operation stack') stack

stackOp' :: Int -> (Stack -> Either ResultCode Stack) -> Stack -> Either ResultCode Stack
stackOp' count operation stack =
  if length stack < count
    then Left $ Error $ "operation failed because there are less than " ++ show count ++ " element(s) on the stack"
    else operation stack

execIfBlock :: (Bool -> Bool) -> Machine -> Result
execIfBlock _ machine@(Machine _ _ [] _) =
  Result (Error "operation failed because there is no element on the stack") machine

execIfBlock condOp machine@(Machine (_:xs) kr (y:ys) as) =
  case inlineIfBlock condOp (bci2Bin y) xs of
    Left rc -> Result rc machine
    Right xs' -> exec (Machine xs' kr ys as)

inlineIfBlock :: (Bool -> Bool) -> B.ByteString -> Program -> Either ResultCode Program
inlineIfBlock condOp condition program =
  let 
    (ifblock, rest) = List.span isEndif program
    (ifPart, elsePart) = List.span isElse ifblock
  in
    if null rest
      then Left (Error "OP_ENDIF is missing")
      else let rest' = tail rest in
        if condOp (isTrue (bin2Bci condition))
          then Right $ (ifPart ++ rest')
        else if not (null elsePart)
          then Right $ (tail elsePart) ++ rest'
          else Right $ rest'

isEndif :: Opcode -> Bool
isEndif OP_ENDIF = True
isEndif _ = False

isElse :: Opcode -> Bool
isElse OP_ELSE = True
isElse _ = False

-- utils {{{2

topIsTrue :: [BCI] -> Bool
topIsTrue (x:_) = isTrue x
topIsTrue _ = False

