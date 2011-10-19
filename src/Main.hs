module Main where

import Language.Bitcoin.Opcodes

main::IO()
main = putStrLn $ show $ fromEnum OP_PUSHDATA1