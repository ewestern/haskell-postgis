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


wkbZ =  0x80000000 :: Word32
wkbM = 0x40000000 :: Word32
wkbSRID = 0x20000000 :: Word32
ewkbTypeOffset = 0x1fffffff :: Word32

toHexDouble :: Double -> BS.ByteString
toHexDouble = BL.toStrict . toLazyByteString . doubleHexFixed 

-- toHexWord should only take 
toHexWord :: Integral a => a -> BS.ByteString
toHexWord i = case  packHexadecimal i of
  Just bs -> bs
  Nothing -> error "Cannot convert negative int"

toHexWord32 :: Integral a => a -> BS.ByteString
toHexWord32 = BL.toStrict . toLazyByteString . int32HexFixed . fromIntegral

toHexInt :: Integral a => a -> BS.ByteString
toHexInt = BL.toStrict . toLazyByteString . int32HexFixed . fromIntegral
{-toHexInt i = case packHexadecimal i of-}
    {-Just bs -> bs-}
    {-Nothing -> error "Cannot create bytestring"-}

fromHexInt :: Integral a => BS.ByteString -> a
fromHexInt bs = case readHexadecimal bs of
    Just (v, r) -> v
    Nothing -> error "Cannot parse hexadecimal"

readLittleEndian :: BS.ByteString -> BS.ByteString
readLittleEndian bs = BS.concat . reverse $ splitEvery bs
  where
    splitEvery bs = 
      let (first, rest) = BS.splitAt 2 bs in 
      if BS.null bs then [] else first : (splitEvery rest)
 

