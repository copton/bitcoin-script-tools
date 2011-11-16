module Language.Bitcoin.Disassembler
(
  run_disassembler
) where

import Control.Monad.Error ()
import Language.Bitcoin.Numbers (bin2Bci)
import Language.Bitcoin.Opcodes (opcodes')
import Language.Bitcoin.Types
import qualified Data.ByteString as B

run_disassembler :: Binary -> Either String Program -- {{{1
run_disassembler binary
  | B.null binary = Right []
  | opcode > 0 && opcode <= 75 = do
      (data_, rest') <- safeSplit (fromIntegral opcode) rest
      program <- run_disassembler rest'
      return $ OP_PUSHDATA Implicit (fromIntegral opcode) (bin2Bci data_) : program
  | opcode == 76 = disassemblePushdata 1 OneByte
  | opcode == 77 = disassemblePushdata 2 TwoBytes
  | opcode == 78 = disassemblePushdata 4 FourBytes
  | otherwise = case lookup opcode opcodes' of
      Nothing -> Left $ "unknown opcode: " ++ show opcode
      Just opcode' -> do
        program <- run_disassembler rest
        return $ opcode' : program
  where
    opcode = B.head binary
    rest = B.tail binary
    disassemblePushdata sizeofLenField pushType = do
      (len, rest') <- safeSplit sizeofLenField rest
      (data_, rest'') <- safeSplit (fromIntegral (bin2Bci len)) rest'
      program <- run_disassembler rest''
      return $ OP_PUSHDATA pushType (hexToInt len) (bin2Bci data_) : program

hexToInt :: B.ByteString -> Integer
hexToInt = B.foldl hexToInt' 0
  where
    hexToInt' s x = s * 16 + (fromIntegral x)

safeSplit :: Int -> B.ByteString -> Either String (B.ByteString, B.ByteString)
safeSplit index bytes
  | B.length bytes < index = Left "unexpected end of binary"
  | otherwise = Right $ B.splitAt index bytes
  
