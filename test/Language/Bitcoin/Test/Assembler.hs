module Language.Bitcoin.Test.Assembler
(
  test
) where

import Control.Monad (liftM)
import qualified Data.ByteString as B
import Language.Bitcoin.Types hiding (Result)
import Language.Bitcoin.Assembler (run_assembler)
import Language.Bitcoin.Disassembler (run_disassembler)
import Language.Bitcoin.Numbers (bin2Bci)
import Language.Bitcoin.Opcodes (opcodes, smallestPushType)
import Test.QuickCheck hiding (reason)
import Test.QuickCheck.Property (liftBool, failed, reason)

test :: IO Result
test = quickCheckResult prop_Bijective

instance Arbitrary Opcode where
  arbitrary = oneof [trivialOpcode, pushdataOpcode]
    where
      trivialOpcode = elements $ filter isRegular $ map fst opcodes
      pushdataOpcode = do
        len <- arbitrary `suchThat` (>0)
        data_ <- liftM (bin2Bci . B.pack) (vector len)
        return $ OP_PUSHDATA (smallestPushType data_) (fromIntegral len) data_
   
prop_Bijective p =
  classify (not (any isPushData p)) "trivial" $
  case run_assembler p >>= run_disassembler of
    Right p' -> liftBool $ p == p'
    Left what -> failed { reason = what }

isPushData (OP_PUSHDATA _ _ _) = True
isPushData _ = False

isRegular OP_FALSE = False
isRegular OP_TRUE = False
isRegular _ = True
