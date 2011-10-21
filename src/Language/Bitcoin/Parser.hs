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

-- run_parser :: String -> Code -> Either String Script {{{1
run_parser :: String -> Code -> Either String Script
run_parser source code =
  case parse script source code of
    Left parseError -> Left $ show parseError
    Right x -> Right x

script :: Parser Script
script = spaces >> endBy operation separator

operation :: Parser Opcode
operation = opcode <|> paste

opcode :: Parser Opcode
opcode = do
  prefix <- string "OP_"
  suffix <- many alphaNum
  liftReadS reads $ prefix ++ suffix

paste :: Parser Opcode
paste = do
  _ <- string "PASTE" >> space >> string "0x"
  value <- many alphaNum
  data_ <- (liftReadS readHex value) :: Parser Integer
  return $ PASTE Nothing $ data_

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

