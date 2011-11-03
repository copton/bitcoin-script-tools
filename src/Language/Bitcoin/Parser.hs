module Language.Bitcoin.Parser
-- export {{{1
(
    run_parser
) where

-- import {{{1
import Language.Bitcoin.Types
import Language.Bitcoin.Utils (b2i, bsLength)
import Text.ParserCombinators.Parsec (Parser, parse, sepEndBy, eof, many, (<|>), (<?>), alphaNum, char, hexDigit, newline, unexpected, satisfy)
import Text.Parsec.Prim (parserFail)
import Control.Monad (liftM, when)
import qualified Data.ByteString as B
import qualified Data.Char as C
import Data.Word (Word8)
import Data.Int (Int32)
import Data.Char (isSpace)
import Data.Maybe (catMaybes)

-- run_parser :: String -> Code -> Either String Script {{{1
run_parser :: String -> Code -> Either String Script
run_parser source code =
  case parse script source code of
    Left parseError -> Left $ show parseError
    Right x -> Right x

script :: Parser Script
script = do
  ops <- liftM catMaybes $ sepEndBy operation separator
  spaces >> eof
  return ops

separator :: Parser Char
separator =  spaces >> (newline <|> char ';')

spaces :: Parser String
spaces = many (satisfy (\c -> isSpace c && not (c =='\n')))

operation :: Parser (Maybe Command)
operation = do
  command <- spaces >> many (alphaNum <|> char '_' <?> "opcode") 
  if command == ""
    then return Nothing
    else liftM Just $ case command of
      "DATA" -> spaces >> liftM DATA hexString
      "KEY" -> keyOrSig KEY
      "SIG" -> keyOrSig SIG
      "OP_PUSHDATA" -> push
      "OP_PUSHDATA1" -> push1
      "OP_PUSHDATA2" -> push2
      "OP_PUSHDATA4" -> push4
      x -> opcode x

keyOrSig :: (Int32 -> Command) -> Parser Command
keyOrSig createCommand = do
  number <- spaces >> hexString
  case b2i number of
    Left e -> parserFail e
    Right value -> return $ createCommand value

opcode :: String -> Parser Command
opcode x = liftM CmdOpcode $ liftReadS reads x

push :: Parser Command
push = pushN checkLength checkValue Direct
  where
    checkLength len = when (len /= 1) $ parserFail "OP_PUSHDATA expects a one byte size parameter"
    checkValue value = when (value > 0x75) $ parserFail "OP_PUSHDATA only support up to 0x75 bytes of data"

push1 :: Parser Command
push1 = pushN checkLength checkValue OneByte
  where
    checkLength len = when (len /= 1) $ parserFail "OP_PUSHDATA1 expects a one byte size parameter"
    checkValue _ = return ()

push2 :: Parser Command
push2 = pushN checkLength checkValue TwoBytes
  where
    checkLength len = when (len /= 2) $ parserFail "OP_PUSHDATA2 expects a two bytes size parameter"
    checkValue _ = return ()
  
push4 :: Parser Command
push4 = pushN checkLength checkValue FourBytes
  where
    checkLength len = when (len /= 4) $ parserFail "OP_PUSHDATA4 expects a four bytes size parameter"
    checkValue _ = return ()

pushN :: (Int -> Parser ()) -> (Int32 -> Parser ()) -> PushDataType -> Parser Command
pushN checkLength checkValue pushType = do
  sizeString <- spaces >> hexString
  checkLength $ bsLength sizeString
  sizeValue <- liftError $ b2i sizeString
  when (sizeValue == 0) $
    parserFail "data of zero length is not allowed"
  checkValue sizeValue
  dataString <- spaces >> hexString
  let dataLength = fromIntegral $ bsLength dataString
  when (dataLength /= sizeValue) $
    parserFail $ "actual length of data does not match the announced length (" ++ show dataLength ++ " vs. " ++ show sizeValue ++ ")"
  return $ CmdOpcode $ OP_PUSHDATA pushType dataString
    
  
hexString :: Parser B.ByteString
hexString = liftM B.pack $ many hexByte

hexByte :: Parser Word8
hexByte = do
  upperNibble <- hexDigit
  lowerNibble <- hexDigit
  return $ fromIntegral $ (C.digitToInt upperNibble) * 16 + (C.digitToInt lowerNibble)

liftReadS :: ReadS a -> String -> Parser a
liftReadS f s =
  let readings = f s in
  if length readings /= 1 || snd (head readings) /= ""
    then unexpected s
    else return $ fst $ head readings

liftError :: Either String a -> Parser a
liftError (Left e) = parserFail e
liftError (Right v) = return v
