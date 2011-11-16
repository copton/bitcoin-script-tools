module Language.Bitcoin.Assembler
-- export {{{1
(
	run_assembler
) where

-- imports {{{1
import Control.Applicative ((<$>), (<*>))
import Control.Monad (when)
import Control.Monad.Error ()
import Data.Word (Word8)
import Language.Bitcoin.Opcodes (opcodes, limit, pushOpcode)
import Language.Bitcoin.Types
import Language.Bitcoin.Numbers (bci2Bin)
import qualified Data.ByteString as B

run_assembler :: Program -> Either String Binary -- {{{1
run_assembler [] = Right B.empty
run_assembler (op@(OP_PUSHDATA _ _ _):xs) = B.append <$> assemblePush op <*> run_assembler xs
run_assembler (x:xs) = B.cons <$> assembleOpcode x <*> run_assembler xs

assembleOpcode :: Opcode -> Either String Word8
assembleOpcode opcode = case lookup opcode opcodes of
  Nothing -> Left "internal error: unknown opcode"
  Just x -> Right x

assemblePush :: Opcode -> Either String B.ByteString
assemblePush (OP_PUSHDATA pushType len value) =
  let
    bytes = bci2Bin value
    len' = B.length bytes
    padding = (fromIntegral len) - len'
    opcode = case pushOpcode pushType of
      Nothing -> B.empty
      Just x -> B.singleton x
    lenfield = pad sizeofLenfield $ intToHex len
    bytes' = pad padding bytes
  in do
    when (padding < 0) $ Left "value of len field is smaller than actual number of bytes"
    when (len > limit pushType) $ Left "invalid length field for OP_PUSHDATA"
    return $ B.concat [opcode, lenfield, bytes']
  where
    sizeofLenfield = case pushType of
      Implicit -> 1
      OneByte -> 1
      TwoBytes -> 2
      FourBytes -> 4

assemblePush _ = error "unexpected parameters"

intToHex :: Integer -> B.ByteString
intToHex number = B.pack $ reverse $ intToHex' number
  where
    intToHex' 0 = []
    intToHex' n = let (q,r) = n `quotRem` 16 in (fromIntegral r) : intToHex' q
  
pad :: Int -> B.ByteString -> B.ByteString
pad count bytes = B.pack (replicate count 0) `B.append` bytes
