module Database.Postgis.Utils where

import qualified Data.ByteString as BS
import Database.Postgis.Geometry
import qualified Data.Vector as V
import System.Endian
import Data.ByteString.Lex.Integral
import Data.Int


wkbZ =  0x80000000 :: Int8
wkbM = 0x40000000 :: Int8
wkbSRID = 0x20000000 :: Int8
ewkbTypeOffset = 0x1fffffff :: Int8
textHex = 0xC0000007 :: Int8

toHexInt :: Integral a => a -> BS.ByteString
toHexInt i = case packHexadecimal i of
    Just bs -> bs
    Nothing -> error "Cannot create bytestring"

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
 

