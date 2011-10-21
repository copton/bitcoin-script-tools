module Language.Bitcoin.Parser
-- export {{{1
(
    run_parser
  , run_printer
) where

-- import {{{1
import Language.Bitcoin.Types
import Numeric (readHex)
import Text.ParserCombinators.Parsec
import Control.Monad (liftM, when)

-- run_parser :: String -> Code -> Either String Script {{{1
run_parser :: String -> Code -> Either String Script
run_parser source code =
  case parse script source code of
    Left parseError -> Left $ show parseError
    Right x -> Right x

script :: Parser Script
script = do
  spaces
  ops <- endBy operation separator
  eof
  return ops

operation :: Parser Command
operation = do
  command <- many (alphaNum <|> char '_' <?> "opcode") 
  case command of
    "DATA" -> spaces >> liftM DATA hex
    "KEY" -> spaces >> hex >>= (\num -> return $ KEY (fromIntegral num))
    "SIG" -> spaces >> hex >>= (\num -> return $ SIG (fromIntegral num))
    "PUSH" -> push
    "OP_PUSHDATA1" -> push1
    "OP_PUSHDATA2" -> push2
    "OP_PUSHDATA4" -> push4
    x -> opcode x

opcode :: String -> Parser Command
opcode x = liftM CmdOpcode $ liftReadS reads x

push :: Parser Command
push = do
  spaces
  value <- hex
  when (value < 1 || value > 0x75) $
    fail "illegal value for PUSH operation (allowed [0x01,0x75])"
  return $ CmdOpcode $ PUSH value

push1 :: Parser Command
push1 = pushN 0xff "OP_PUSHDATA1" "exactly one byte" OP_PUSHDATA1

push2 :: Parser Command
push2 = pushN 0xffff "OP_PUSHDATA2" "at most two bytes" OP_PUSHDATA2

push4 :: Parser Command
push4 = pushN 0xffffffff "OP_PUSHDATA4" "at most four bytes" OP_PUSHDATA4

pushN :: Num a => Integer -> String -> String -> (a -> Integer -> Opcode) -> Parser Command
pushN maxSize opcodeString errorString createOpcode = do
  size <- spaces >> hex
  when (size > maxSize) $
    fail $ "size parameter for " ++ opcodeString ++ " must be " ++ errorString
  value <- spaces >> hex
  when (value > 2 ^ (8*size)) $
    fail $ "data is more than " ++ show size ++ " byte(s)"
  return $ CmdOpcode $ createOpcode (fromIntegral size) value

hex :: Parser Integer
hex = many alphaNum >>= liftReadS readHex

separator :: Parser Char
separator = newline <|> char ';'

-- utils {{{1
liftReadS :: ReadS a -> String -> Parser a
liftReadS f s =
  let readings = f s in
  if length readings /= 1 || snd (head readings) /= ""
    then unexpected s
    else return $ fst $ head readings

-- run_printer :: Script -> Code {{{1
run_printer :: Script -> Code
run_printer = undefined

