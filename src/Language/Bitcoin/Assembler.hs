module Language.Bitcoin.Assembler
-- export {{{1
(
	run_assembler
) where

-- imports {{{1
import Control.Applicative ((<$>), (<*>))
import Control.Monad.Error ()
import Data.Word (Word8)
import Language.Bitcoin.Opcodes (opcodes, limit)
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
    bytes' = pad (fromIntegral(len) - len') bytes
  in if (len <= limit pushType)
    then Right $ (bci2Bin . fromIntegral) len `B.append` bytes'
    else Left "invalid length field for OP_PUSHDATA"

assemblePush _ = error "unexpected parameters"

pad :: Int -> B.ByteString -> B.ByteString
pad count bytes
  | count < 0 = error "internal error: unexpected parameter"
  | otherwise = B.pack (replicate count 0) `B.append` bytes

