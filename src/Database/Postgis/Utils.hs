module Database.Postgis.Utils where

import qualified Data.ByteString as BS
import Database.Postgis.Geometry
import qualified Data.Vector as V
import System.Endian
import Data.ByteString.Lex.Integral
import Data.ByteString.Builder
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BL
import Data.Word
import Data.Binary.IEEE754
import Data.Int

wkbZ =  0x80000000 :: Word32
wkbM = 0x40000000 :: Word32
wkbSRID = 0x20000000 :: Word32
ewkbTypeOffset = 0x1fffffff :: Word32



builderToBS :: Builder -> BS.ByteString
builderToBS = BL.toStrict . toLazyByteString

toHexNum :: (a -> Builder) -> a -> BS.ByteString
toHexNum conv = case getSystemEndianness of
  BigEndian -> builderToBS . conv
  LittleEndian -> convertLittleEndian . builderToBS . conv
  
toHexWord64 :: Word64 -> BS.ByteString
-- These two have the same outcome
{-toHexWord64 = builderToBS . word64HexFixed -}
{-toHexWord64 w = case packHexadecimal w of-}
  {-Nothing -> error "Negative input"-}
  {-Just bs -> bs-}

toHexWord64 = case getSystemEndianness of
  BigEndian -> builderToBS . word64HexFixed
  LittleEndian -> builderToBS . word64HexFixed . byteSwap64

-- this doesn't need to byteswap
toHexInt8 :: Int8 -> BS.ByteString
toHexInt8 = builderToBS . int8HexFixed 

toHexWord32 :: Word32 -> BS.ByteString
toHexWord32 = case getSystemEndianness of 
  BigEndian -> builderToBS . word32HexFixed 
  LittleEndian -> builderToBS . word32HexFixed . byteSwap32


toHexDouble :: Double -> BS.ByteString
toHexDouble d = case packHexadecimal $ doubleToWord d of
  Nothing -> error "Cannot"
  Just b -> b

fromHexInt :: Integral a => BS.ByteString -> a
fromHexInt bs = case readHexadecimal bs of
    Just (v, r) -> v
    Nothing -> error "Cannot parse hexadecimal"

convertLittleEndian :: BS.ByteString -> BS.ByteString
convertLittleEndian bs = BS.concat . reverse $ splitEvery bs
  where
    splitEvery bs = 
      let (first, rest) = BS.splitAt 2 bs in 
      if BS.null bs then [] else first : (splitEvery rest)
