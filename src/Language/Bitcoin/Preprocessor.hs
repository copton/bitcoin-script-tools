module Language.Bitcoin.Preprocessor
-- export {{{1
(
  run_preprocessor
) where

-- import {{{1
import Language.Bitcoin.Types
import Language.Bitcoin.Numbers
import qualified Data.ByteString as B
import qualified Data.List as List

-- run_preprocessor :: Script -> (Program, Keyring) {{{1
run_preprocessor :: Script -> (Program, Keyring)
run_preprocessor script = foldr process ([], []) script


process :: Command -> (Program, Keyring) -> (Program, Keyring)
process (CmdOpcode op) (program, keyring) = (op : program, keyring)
process (KEY number) x = processKey keyPublic number x
process (SIG number) x = processKey keyPrivate number x
process (DATA data_) (program, keyring) = (push data_ : program, keyring)


processKey :: (Keypair -> BCI) -> BCI -> (Program, Keyring) -> (Program, Keyring)
processKey getter number (program, keyring) =
  let (keyring', keypair) = getOrCreate keyring number in
  (OP_PUSHDATA Direct (getter keypair) : program, keyring')


getOrCreate :: Keyring -> BCI -> (Keyring, Keypair)
getOrCreate keyring publicKey =
  case List.find ((== publicKey) . keyPublic) keyring of
    Nothing ->
      let
        privateKey = negate publicKey
        keypair = Keypair publicKey privateKey
      in
        (keypair : keyring, keypair)
    Just keypair -> (keyring, keypair)


push :: BCI -> Opcode
push data_ = OP_PUSHDATA (pushType (B.length data_)) (bci2Bin data_)
  where
    pushType size
      | size == 0 = error "internal error"
      | size <= 75 = Direct
      | size <= 0xff = OneByte
      | size <= 0xffff = TwoBytes
      | otherwise = FourBytes
