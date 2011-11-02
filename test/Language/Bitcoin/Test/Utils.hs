module Language.Bitcoin.Test.Utils
(
  tests
) where

import Language.Bitcoin.Utils (b2i', i2b, unpad)
import qualified Data.ByteString.Lazy as B
import Test.HUnit

b2iTestCases = [
    ([0x23], 0x23)
  , ([0x12, 0x34], 0x1234)
  , ([0x12, 0x34, 0x56], 0x123456)
  , ([0x12, 0x34, 0x56, 0x78], 0x12345678)
  , ([0x70, 0x00, 0x00, 0x01], -1)
  , ([0x82, 0x34, 0x56, 0x78], -0x12345678)
  , ([0], 0)
  , ([0x70, 0x00, 0x00, 0x00], 0)
  ]

i2bTestCases = [
    (0x23, [0x00, 0x00, 0x00, 0x23])
  , (0x1234, [0x00, 0x00, 0x12, 0x34])
  , (0x123456, [0x00, 0x12, 0x34, 0x56])
  , (0x12345678, [0x12, 0x34, 0x56, 0x78])
  , (-1, [0x70, 0x00, 0x00, 0x01])
  , (-0x12345678, [0x82, 0x34, 0x56, 0x78])
  , (0, [0x00, 0x00, 0x00, 0x00])
  ]

tests = TestLabel "Utils" $ TestList $
  b2iTests ++ i2bTests

b2iTests :: [Test]
b2iTests = map runTest b2iTestCases
  where
    runTest (input, expected) = TestCase $
      expected @=? (b2i' $ B.pack input)

i2bTests :: [Test]
i2bTests = map runTest i2bTestCases
  where
    runTest (input, expected) = TestCase $
      expected @=? (B.unpack $ i2b input)

-- avoid swap from Data.Tuple. It has been added to base 4.3 and GHC 6 uses base < 4.3
swap (x, y) = (y, x)
