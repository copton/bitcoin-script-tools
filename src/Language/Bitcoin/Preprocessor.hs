module Language.Bitcoin.Preprocessor
(
  run_preprocessor
) where

import Language.Bitcoin.Types

run_preprocessor :: Script -> Either String Program
run_preprocessor script = Right $ foldr process [] script

process :: Command -> Program -> Program
process (CmdOpcode op) program = op : program
process _ _ = error "TODO"
