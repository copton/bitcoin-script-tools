module Language.Bitcoin.Preprocessor
-- export {{{1
(
  run_preprocessor
) where

-- import {{{1
import Control.Arrow (second)
import Language.Bitcoin.Types
import Language.Bitcoin.Numbers
import qualified Data.Map as Map
import qualified Data.ByteString as B

type Keys = Map.Map Int Keypair

run_preprocessor :: Script -> (Program, Keyring) -- {{{1
run_preprocessor script = second (Map.elems) $ foldr process ([], Map.empty) script

process :: Command -> (Program, Keys) -> (Program, Keys)
process (CmdOpcode op) (program, keys) = (op : program, keys)
process (KEY keyId) x = processKey keyPublic keyId x
process (SIG keyId) x = processKey keyPrivate keyId x
process (DATA data_) (program, keyring) = (push data_ : program, keyring)

processKey :: (Keypair -> BCI) -> Int -> (Program, Keys) -> (Program, Keys)
processKey getter keyId (program, keys) =
  let (keys', keypair) = getOrCreate keys keyId in
  (OP_PUSHDATA Direct 64 (getter keypair) : program, keys') -- TODO: set the right length

getOrCreate :: Keys-> Int -> (Keys, Keypair)
getOrCreate keys keyId =
  case Map.lookup keyId keys of
    Nothing -> -- TODO: generate a new key pair
      let keypair = Keypair (fromIntegral keyId) (negate (fromIntegral keyId)) in
      (Map.insert keyId keypair keys, keypair) 
    Just keypair ->
      (keys, keypair)


push :: BCI -> Opcode
push data_ = OP_PUSHDATA pushType ((fromIntegral . B.length . bci2Bin) data_) data_
  where
    pushType
      | data_ < 2^(8*75) = Direct
      | data_ < 2^(8*0xff) = OneByte
      | data_ < 2^(8*0xffff) = TwoBytes
      | otherwise = FourBytes
