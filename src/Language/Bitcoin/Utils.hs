module Language.Bitcoin.Utils
(
  b2i, b2i', i2b, bsLength, pad, unpad, bs
) where

import qualified Data.ByteString.Lazy as B
import Control.Exception.Base (assert)
import Data.Int (Int32)
import Data.Ix (inRange)
import Data.Maybe (fromJust)
import Prelude hiding (max, length, sum)

b2i' :: B.ByteString -> Int32
b2i' string = assert (inRange (1,4) (bsLength string)) $
  let
    (sign, bytes) = extractSign $ pad 4 string 
    value =  B.foldl (\sum byte -> sum * 256 + (fromIntegral byte)) 0 bytes
  in
    sign * value

b2i :: B.ByteString -> Either String Int32
b2i string = 
  let length = bsLength string in
  if inRange (1,4) length
    then Right $ b2i' string
    else Left $ "cannot convert " ++ show length ++ " bytes to int"

i2b :: Int32 -> B.ByteString
i2b value =
  let
    (sign, value') = if value < 0
      then (0x70, -1 * value)
      else (0, value)
    bytes = pad 4 $ bs (fromIntegral value')
    (msb, rest) = fromJust $ B.uncons bytes
  in
    (msb + sign) `B.cons` rest    
     
bs :: Integer -> B.ByteString
bs 0 = B.pack [0]
bs x = B.reverse $ bs' x
  where
    bs' 0 = B.empty
    bs' value =
      let (q, r) = quotRem value 256 in
      B.cons (fromIntegral r) (bs' q)

bsLength :: B.ByteString -> Int
bsLength string =
  let
    length = B.length string
    max = fromIntegral (maxBound :: Int)
  in
    assert (length <= max) (fromIntegral length)

pad :: Int -> B.ByteString -> B.ByteString
pad size string =
  let length = bsLength string in
  if length < size
    then B.append (B.pack $ replicate (size - length) 0) string
    else string

unpad :: B.ByteString -> B.ByteString
unpad string
  | B.null string = string
  | B.head string == 0 = unpad $ B.tail string
  | otherwise = string

extractSign :: B.ByteString -> (Int32 , B.ByteString)
extractSign string = assert (B.length string == 4) $
  let (msb, rest) = fromJust $ B.uncons string in
  if msb >= 0x70
    then (-1, (msb - 0x70) `B.cons` rest)
    else (1, msb `B.cons` rest)

