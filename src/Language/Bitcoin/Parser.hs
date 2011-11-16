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
import Language.Bitcoin.Opcodes (limit)
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
        "OP_PUSHDATA" -> push Implicit
        "OP_PUSHDATA1" -> push OneByte
        "OP_PUSHDATA2" -> push TwoBytes
        "OP_PUSHDATA4" -> push FourBytes
        "PUSHDATA" -> push Implicit
        "PUSHDATA1" -> push OneByte
        "PUSHDATA2" -> push TwoBytes
        "PUSHDATA4" -> push FourBytes
        x -> opcode x
  spaces >> return op

dataCmd :: Parser Command
dataCmd = do
  value <- bciValue
  return $ DATA value

keyOrSig :: (Int -> Command) -> Parser Command
keyOrSig createCommand = do
  numberString <- spaces >> many digit
  number <- liftReadS reads numberString 
  return $ createCommand number

opcode :: String -> Parser Command
opcode x = liftM CmdOpcode $ liftReadS reads $ if take 3 x == "OP_" then x else "OP_" ++ x

push :: PushDataType -> Parser Command
push pushType = do
  len <- integer
  when (len <= 0) $ parserFail "the length field of OP_PUSHDATA must be a positive value"
  when (len > limit pushType) $ parserFail $ "the length field of " ++ show pushType ++ " OP_PUSHDATA must be smaller then " ++ show (limit pushType)
  dataString <- spaces >> pushdataString len
  dataValue <- liftReadS reads dataString
  return $ CmdOpcode $ OP_PUSHDATA pushType len dataValue

pushdataString :: Integer -> Parser String
pushdataString len = do
  dataString <- spaces >> many alphaNum
  when (take 2 dataString /= "0x") $ parserFail "the data field of OP_PUSHDATA must be a hexadecimal value"
  let (dataLen, rest) = (length dataString - 2) `quotRem` 2
  when (rest == 1) $ parserFail "the data field of OP_PUSHDATA is missing a nibble"
  when (dataLen /= fromIntegral len) $
    parserFail $ "actual length of data does not match the announced length (" ++ show dataLen ++ " vs. " ++ show len ++ ")"
  return dataString

bciValue :: Parser BCI
bciValue = do
  string <- spaces >> many alphaNum
  liftReadS reads string

integer :: Parser Integer
integer = do
  digits <- spaces >> many digit
  liftReadS reads digits  

liftReadS :: ReadS a -> String -> Parser a
liftReadS f s =
  let readings = f s in
  if length readings /= 1 || snd (head readings) /= ""
    then unexpected s
    else return $ fst $ head readings
