module Language.Bitcoin.Parser
-- export {{{1
(
    run_parser
) where

-- import {{{1
import Control.Monad (liftM, when)
import Data.Char (isSpace, toUpper)
import Data.Maybe (catMaybes) 
import Language.Bitcoin.Types
import Language.Bitcoin.Numbers (BCI)
import qualified Data.List as List
import Text.Parsec.Prim (parserFail)
import Text.ParserCombinators.Parsec (Parser, parse, sepEndBy, eof, many, (<|>), (<?>), alphaNum, digit, char, newline, unexpected, satisfy)

-- run_parser :: String -> Code -> Either String Script {{{1
run_parser :: String -> Code -> Either String Script
run_parser source code =
  case parse script source (removeComments code) of
    Left parseError -> Left $ show parseError
    Right x -> Right x

removeComments :: String -> String
removeComments [] = []
removeComments string =
  let
    (code, rest) = List.break (=='#') string 
    (_, string') = List.break (=='\n') rest
  in code ++ removeComments string'

script :: Parser Script
script = do
  ops <- spaces >> (liftM catMaybes $ sepEndBy operation separator)
  eof
  return ops

separator :: Parser ()
separator = (newline <|> char ';') >> spaces >> return ()

spaces :: Parser String
spaces = many (satisfy (\c -> isSpace c && not (c =='\n')))

operation :: Parser (Maybe Command)
operation = do
  op <- do
    command <- many (alphaNum <|> char '_' <?> "opcode") 
    if command == ""
      then return Nothing
      else liftM Just $ case map toUpper command of
        "DATA" -> dataCmd
        "KEY" -> keyOrSig KEY
        "SIG" -> keyOrSig SIG
        "OP_PUSHDATA" -> push
        "OP_PUSHDATA1" -> push1
        "OP_PUSHDATA2" -> push2
        "OP_PUSHDATA4" -> push4
        "PUSHDATA" -> push
        "PUSHDATA1" -> push1
        "PUSHDATA2" -> push2
        "PUSHDATA4" -> push4
        x -> opcode x
  spaces >> return op

dataCmd :: Parser Command
dataCmd = do
  (_, value) <- bciValue
  return $ DATA value

keyOrSig :: (Int -> Command) -> Parser Command
keyOrSig createCommand = do
  numberString <- spaces >> many digit
  number <- liftReadS reads numberString 
  return $ createCommand number

opcode :: String -> Parser Command
opcode x = liftM CmdOpcode $ liftReadS reads $ if take 3 x == "OP_" then x else "OP_" ++ x

push :: Parser Command
push = pushN checkLength checkValue Direct
  where
    checkLength len = when (len /= 1) $ parserFail "OP_PUSHDATA expects a one byte size parameter"
    checkValue value = when (value > 75) $ parserFail "OP_PUSHDATA only support up to 75 bytes of data"

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

pushN :: (BCI -> Parser ()) -> (Int -> Parser ()) -> PushDataType -> Parser Command
pushN checkLength checkValue pushType = do
  (sizeLength, size) <- bciValue
--  checkLength sizeLength
  (dataLength, data_) <- bciValue
--  checkValue $ realSize
--  when (realSize /= fromIntegral size) $
--    parserFail $ "actual length of data does not match the announced length (" ++ show realSize ++ " vs. " ++ show size ++ ")"
  return $ CmdOpcode $ OP_PUSHDATA pushType data_

bciValue :: Parser (Int, BCI)
bciValue = do
  string <- spaces >> many alphaNum
  value <- liftReadS reads string
  return (length string, value) 

--hexString :: Parser B.ByteString
--hexString = liftM B.pack $ many hexByte

--hexByte :: Parser Word8
--hexByte = do
--  upperNibble <- hexDigit
--  lowerNibble <- hexDigit
--  return $ fromIntegral $ (C.digitToInt upperNibble) * 16 + (C.digitToInt lowerNibble)

liftReadS :: ReadS a -> String -> Parser a
liftReadS f s =
  let readings = f s in
  if length readings /= 1 || snd (head readings) /= ""
    then unexpected s
    else return $ fst $ head readings
