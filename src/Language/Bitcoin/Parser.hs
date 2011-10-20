module Language.Bitcoin.Parser
-- export {{{1
(
	run_parser, run_printer
) where

-- import {{{1
import Language.Bitcoin.Types
import Text.ParserCombinators.Parsec
import qualified Data.ByteString.Char8 as B

-- run_parser :: Code -> Script {{{1
run_parser :: Code -> Either ParseError Script
run_parser code = parse script "(unknown)" (B.unpack code)


script :: Parser Script
script = sepBy operation separator

operation :: Parser Opcode
operation = opcode -- <|> paste

opcode :: Parser Opcode
opcode = string "OP_FALSE" >> return OP_FALSE
--paste = string "PASTE" >> bytes >>= (\bs -> return PASTE DATA (pack bs))

separator :: Parser Char
separator = char '\n' -- <|> char ';'


-- run_printer :: Script -> Code {{{1
run_printer :: Script -> Code
run_printer = undefined

