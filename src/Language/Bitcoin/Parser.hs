module Language.Bitcoin.Parser
-- export {{{1
(
  run_parser, run_parser',
  run_printer
) where

-- import {{{1
import Language.Bitcoin.Types
import Text.ParserCombinators.Parsec
import qualified Data.ByteString.Char8 as B

-- run_parser :: Code -> Script {{{1

run_parser :: String -> Code -> Either ParseError Script
run_parser source code = run_parser' source (B.unpack code)

run_parser' :: String -> String -> Either ParseError Script
run_parser' source code = parse script source code

script :: Parser Script
script = spaces >> endBy operation separator

operation :: Parser Opcode
operation = opcode -- <|> paste

opcode :: Parser Opcode
opcode = do
  prefix <- string "OP_"
  suffix <- many alphaNum
  let op = prefix ++ suffix
  let readings = reads op
  if length readings /= 1 || snd (head readings) /= ""
    then unexpected op <?> "valid opcode"
    else return $ fst $ head readings

--paste = string "PASTE" >> bytes >>= (\bs -> return PASTE DATA (pack bs))

separator :: Parser Char
separator = newline <|> char ';'


-- run_printer :: Script -> Code {{{1
run_printer :: Script -> Code
run_printer = undefined

