{-# OPTIONS_GHC -fno-warn-incomplete-patterns #-}
module Language.Bitcoin.Numbers (
-- ********************************************************************
-- *    PGMID.        BITCOIN NUMBER TYPE MODULE.                     *
-- *    AUTHOR.       BERND R. FIX   >Y<                              *
-- *    DATE WRITTEN. 11/11/09.                                       *
-- *    COPYRIGHT.    (C) BY BERND R. FIX. ALL RIGHTS RESERVED.       *
-- *                  LICENSED MATERIAL - PROGRAM PROPERTY OF THE     *
-- *                  AUTHOR. REFER TO COPYRIGHT INSTRUCTIONS.        *
-- *    REMARKS.      REVISION HISTORY AT END OF FILE.                *
-- ********************************************************************
  BCI
, bin2Bci
, bci2Bin
, isTrue
) where

import qualified Data.ByteString as B
import Data.Word (Word8)
import Data.List (elemIndex)


-- ####################################################################
-- BitCoin arbitrary precision Integers
--     Bloated definition to allow "-0" to be represented.
-- ####################################################################

data BCI = BCI {
      sign :: Bool      -- is negative?
    , value :: Integer  -- absolute value
    }


-- ####################################################################
-- Display BCIs
--     If the absolute value of the integer is in the range
--     [-2^31 .. 2^31-1], it is printed as a numeric value,
--     otherwise as a hex-encoded binary representation.
-- ####################################################################

instance Show BCI where
    show n = showBCI n

showBCI :: BCI -> String
showBCI n = if signedVal >= lower && signedVal <= upper then
        (if signedVal == 0 && (sign n) then "-0" else show signedVal)
    else 
        hexify "0x" (int2Bin (value n) (sign n) [])
    where
        signedVal = bci2Int n
        upper = fromIntegral (maxBound :: Int)
        lower = fromIntegral (minBound :: Int)

hexChars :: String
hexChars = "0123456789ABCDEFabcdef"

hexify :: String -> [Word8] -> String
hexify s [] = s
hexify s (x:xs) = hexify (s ++ (hex $ fromEnum x)) xs where
    hex :: Int -> String
    hex b = (hexChars !! (b `div` 16)) : (hexChars !! (b `mod` 16)) : []


-- ####################################################################
-- Read BCIs from strings
-- ####################################################################

instance Read BCI where
    -- skip leading white spaces
    readsPrec d (' ':s) = readsPrec d s 
    readsPrec d ('\t':s) = readsPrec d s 
    readsPrec d ('\r':s) = readsPrec d s
    readsPrec d ('\n':s) = readsPrec d s
    -- read BCI from hexadecimal representation  
    readsPrec _ ('0':'x':s) = hex2Bci h [] s' where (h,s') = scanHex "" s 
    -- read BCI as signed number
    readsPrec _ ('-':s) = [(BCI True i, s') | (i,s') <- reads s]
    readsPrec _ s = [(BCI False i, s') | (i,s') <- reads s]

hex2Bci :: String -> [Word8] -> String -> [(BCI,String)]
hex2Bci [] [] _ = []
hex2Bci s [] s' = hex2Bci (if odd $ length s then '0':s else s) [0] s'
hex2Bci [] b s' = [(bin2Bci' $ drop 1 b, s')]
hex2Bci (c1:c2:s) b s' = hex2Bci s (b ++ [toEnum(16 * hexVal c1 + hexVal c2)]) s' 

scanHex :: String -> String -> (String,String)
scanHex s [] = (s,"")
scanHex s (x:xs) = if hexVal x < 0
    then (s, x:xs) 
    else scanHex (s ++ [x]) xs

hexVal :: Char -> Int
hexVal c = case elemIndex c hexChars of
    Nothing -> -1
    Just idx -> if idx > 15 then idx - 6 else idx


-- ####################################################################
-- Compare BCIs
-- ####################################################################

instance Eq BCI where
    (==) n1 n2 = (sign n1) == (sign n2) && (value n1) == (value n2)


-- ####################################################################
-- Number instances
-- ####################################################################

instance Num BCI where
    (*) n1 n2 = BCI ((sign n1) /= (sign n2)) ((value n1) * (value n2))
    (+) n1 n2 = int2Bci $ (bci2Int n1) + (bci2Int n2)
    (-) n1 n2 = int2Bci $ (bci2Int n1) - (bci2Int n2)
    abs n = int2Bci $ value n
    signum n = BCI (sign n) 1
    negate n = BCI (not $ sign n) (value n)
    fromInteger i = int2Bci i

instance Enum BCI where
    succ n = int2Bci $ (bci2Int n) + 1
    pred n = int2Bci $ (bci2Int n) - 1
    toEnum i = int2Bci $ fromIntegral i
    fromEnum n = fromIntegral $ bci2Int n

instance Real BCI where
    toRational n = toRational $ bci2Int n

instance Ord BCI where
    compare n1 n2 = compare (bci2Int n1) (bci2Int n2)
    (<) n1 n2 = (bci2Int n1) < (bci2Int n2)
    (<=) n1 n2 = (bci2Int n1) <= (bci2Int n2)
    (>) n1 n2 = (bci2Int n1) > (bci2Int n2)
    (>=) n1 n2 = (bci2Int n1) >= (bci2Int n2)
    min n1 n2 = int2Bci $ min (bci2Int n1) (bci2Int n2)
    max n1 n2 = int2Bci $ max (bci2Int n1) (bci2Int n2)

instance Integral BCI where
    (quot) n1 n2 = int2Bci $ (bci2Int n1) `quot` (bci2Int n2)
    (rem) n1 n2 = int2Bci $ (bci2Int n1) `rem` (bci2Int n2)
    (div) n1 n2 = BCI ((sign n1) /= (sign n2)) ((value n1) `div` (value n2))
    (mod) n1 n2 = int2Bci $ (bci2Int n1) `mod` (bci2Int n2)
    (quotRem) n1 n2 = (quot n1 n2, rem n1 n2)
    (divMod) n1 n2 = (div n1 n2, mod n1 n2)
    toInteger n = bci2Int n


-- ####################################################################
-- Logical operations
-- ####################################################################

isTrue :: BCI -> Bool
isTrue n = n /= BCI False 0


-- ####################################################################
-- Conversion between intrinsic integers and BCIs
-- ####################################################################

int2Bci :: Integer -> BCI
int2Bci i = BCI neg val where
    neg = i < 0
    val = if neg then negate i else i

bci2Int :: BCI -> Integer
bci2Int n = if sign n then negate $ value n else value n


-- ####################################################################
-- Convert binary representation to BCI
-- ####################################################################
-- in:  binary representation 
-- out: BCI

bin2Bci :: B.ByteString -> BCI
bin2Bci bs = bin2Bci' $ B.unpack bs

-- ========================================================
-- Conversion helper #1 {byte array to integer}
-- ========================================================
-- in:  byte array (binary representation)
-- out: integer value

bin2Bci' :: [Word8] -> BCI
bin2Bci' [] = BCI False 0
bin2Bci' (x:xs) = BCI neg (bin2Int (val:xs)) where
    neg =  x > 127
    val = if neg then x - 128 else x

-- ========================================================
-- Conversion helper #2 {byte array to unsigned integer}
-- ========================================================
-- in:  byte array (binary representation)
-- out: unsigned integer value

bin2Int :: [Word8] -> Integer
bin2Int xs = foldl (\v x -> 256 * v + fromIntegral (fromEnum x)) 0 xs


-- ####################################################################
-- Convert BCI to binary representation
-- ####################################################################
-- in:  BCI
-- out: binary representation 

bci2Bin :: BCI -> B.ByteString
bci2Bin n =  B.pack $ int2Bin (value n) (sign n) []

-- ========================================================
-- Conversion helper {integer to binary}
-- ========================================================
-- in:  absolute value (integer)
--      negative value? (bool)
--      byte array to be appended (empty on first call)
-- out: byte array of binary representation

int2Bin :: Integer -> Bool-> [Word8] -> [Word8]
int2Bin 0 s [] = if s then [128] else [0]
int2Bin 0 s (x:xs) = if x > 127 then off:x:xs else (x+off):xs where
    off = if s then 128 else 0
int2Bin n s xs = int2Bin (n `div` 256) s ((lsb n):xs) where
    lsb :: Integer -> Word8
    lsb v = toEnum $ fromIntegral (v `mod` 256)

