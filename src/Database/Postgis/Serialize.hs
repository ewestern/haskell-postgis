{-# LANGUAGE TypeFamilies, GADTs, FlexibleInstances, OverloadedStrings #-}
module Database.Postgis.Serialize  where
import Database.Postgis.Geometry

import Data.ByteString.Lex.Integral
import Data.Bits
import Control.Monad.Reader
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BC
import Data.Binary
import Data.Binary.Get
import Data.Binary.Put
import System.Endian
import qualified Data.Vector as V

import Data.Binary.IEEE754
import Data.Int
import Control.Applicative ((<$>))

wkbZ =  0x80000000 :: Word32
wkbM = 0x40000000 :: Word32
wkbSRID = 0x20000000 :: Word32
ewkbTypeOffset = 0x1fffffff :: Word32


  
writeGeometry :: Geometry -> BL.ByteString
writeGeometry = runPut . putGeometry 

readGeometry :: BL.ByteString -> Geometry
readGeometry = runGet getGeometry

type Getter = ReaderT Header Get 
type Putter a = a -> Put 

instance Binary Endianness where
  get = fromHex <$> (getLazyByteString 2) 
  put = putLazyByteString . toHex

instance Binary Header where
  get = getHeader 
  put (Header bo gt s) = put bo >> (putInt . fromIntegral) gt  >> putMaybe s putInt

class Hexable a where
  toHex :: a -> BL.ByteString
  fromHex :: BL.ByteString -> a

instance Hexable Endianness where
  toHex BigEndian = "00" :: BL.ByteString  
  toHex LittleEndian = "01" :: BL.ByteString  
  fromHex b = case (fromHexInt b) :: Word8 of
    0 -> BigEndian
    1 -> LittleEndian
    _ -> error $ "Not an Endian" ++ show b

instance Hexable Int where
  toHex = (toHexWord  byteSwap32 8) . fromIntegral
  fromHex = fromHexInt

instance Hexable Word32  where
  toHex =  (toHexWord  byteSwap32 8) . fromIntegral
  fromHex = fromHexInt


instance Hexable Double where
  fromHex = wordToDouble . fromHexInt 
  toHex = (toHexWord byteSwap64 16) . doubleToWord

--- converters

endFunc ::(a -> a) -> (a -> a)
endFunc f = case getSystemEndianness of
  BigEndian -> id
  LittleEndian -> f

toHexWord :: Integral a => (a -> a) -> Int -> a -> BL.ByteString
toHexWord f l w = conv . (endFunc f) $ w
  where
    conv w = case packHexadecimal w of
      Just s -> BL.fromChunks [padd l s]
      Nothing -> error "Cannot convert word" 
    padd l bs =
      let diff = l - (BS.length bs )
      in BS.append (BC.replicate diff '0') bs


fromHexInt :: Integral a => BL.ByteString -> a
fromHexInt bs = case readHexadecimal lazy of
    Just (v, r) -> v
    Nothing -> error "Cannot parse hexadecimal"
    where
      lazy = BS.concat . BL.toChunks $ bs

convertLittleEndian :: BL.ByteString -> BL.ByteString
convertLittleEndian bs = BL.concat . reverse $ splitEvery bs
  where
    splitEvery bs = 
      let (first, rest) = BL.splitAt 2 bs in 
      if BL.null bs then [] else first : (splitEvery rest)


data Header = Header {
    _byteOrder :: Endianness
  , _geoType :: Word32
  , _srid :: SRID
} deriving (Show)

---
makeHeader :: EWKBGeometry a => SRID -> a -> Header
makeHeader s geo =
  let gt = geoType geo
      wOr acc (p, h) = if p then h .|. acc else acc
      typ = foldl wOr gt [(hasM geo, wkbM), (hasZ geo, wkbZ), (s /= Nothing, wkbSRID)]   
  in Header getSystemEndianness typ s 


putRing :: Putter LinearRing
putRing v = do
  putInt . V.length $ v  
  V.mapM_ putPoint v

putGeometry :: Putter Geometry
putGeometry (GeoPoint s p) = do
  put $ makeHeader s p
  putPoint p

putGeometry (GeoLineString s ls@(LineString v)) = do
  put $ makeHeader s ls 
  putRing v

putGeometry (GeoPolygon s pg@(Polygon rs)) = do
  put $ makeHeader s pg
  putInt . V.length $ rs 
  V.mapM_ putRing rs

putGeometry (GeoMultiPoint s mp@(MultiPoint ps)) = do
  put $ makeHeader s mp
  putInt . V.length $ ps 
  V.mapM_ (putGeometry . GeoPoint s)  ps

putGeometry (GeoMultiLineString s mls@(MultiLineString ls)) = do
  put $ makeHeader s mls
  putInt . V.length $ ls 
  V.mapM_ (putGeometry . GeoLineString s) ls

putGeometry (GeoMultiPolygon s mpg@(MultiPolygon ps)) = do
  put $ makeHeader s mpg
  putInt . V.length $  ps 
  V.mapM_ (putGeometry . GeoPolygon s)  ps

----
putPoint :: Putter Point
putPoint (Point x y m z) = putDouble x >> putDouble y >> putMaybe m putDouble >> putMaybe z putDouble

putDouble :: Putter Double
putDouble = putLazyByteString . toHex

putInt :: Putter Int
putInt = putLazyByteString . toHex 

putMaybe :: Maybe a -> Putter a -> Put
putMaybe mi = case mi of
  Just i -> ($ i) 
  Nothing -> (\x -> return ())

--
-- getters 
--
getGeometry :: Get Geometry 
getGeometry = do
  h <- lookAhead get
  let t = (_geoType h) .&. ewkbTypeOffset
      mkGeo :: (SRID -> a -> Geometry) -> Getter a -> Get Geometry
      mkGeo cons p = cons (_srid h) <$> runReaderT p h
  case t of
      1 -> mkGeo GeoPoint getGeoPoint
      2 -> mkGeo GeoLineString getLineString 
      3 -> mkGeo GeoPolygon getPolygon 
      4 -> mkGeo GeoMultiPoint getMultiPoint 
      5 -> mkGeo GeoMultiLineString getMultiLineString
      6 -> mkGeo GeoMultiPolygon  getMultiPolygon 
      _ -> error $ "not yet implemented" ++ (show t)


getMultiPolygon :: Getter MultiPolygon 
getMultiPolygon = do 
  lift getHeader
  n <- getInt
  ps <- V.replicateM n getPolygon
  return $ MultiPolygon ps

getMultiLineString :: Getter MultiLineString 
getMultiLineString = do
  lift getHeader
  n <- getInt
  ls <- V.replicateM n getLineString 
  return $ MultiLineString ls

getMultiPoint :: Getter MultiPoint 
getMultiPoint = do
  lift getHeader
  n <- getInt 
  ps <- V.replicateM n getGeoPoint
  return $ MultiPoint ps
 
getPolygon :: Getter Polygon 
getPolygon = lift getHeader >> Polygon <$> (getInt >>= (\n -> V.replicateM n getRing))

getLineString :: Getter LineString 
getLineString = lift getHeader >> LineString <$> getSegment

getRing :: Getter LinearRing
getRing = getSegment 

getSegment :: Getter (V.Vector Point)
getSegment = getInt >>= (\n -> V.replicateM n getPoint) 
 
getGeoPoint :: Getter Point
getGeoPoint = lift getHeader >> getPoint

getPoint :: Getter Point
getPoint = do
    gt <- asks _geoType 
    let hasM = if (gt .&. wkbM) > 0 then True else False 
        hasZ = if (gt .&. wkbZ) > 0 then True else False
    x <- getDouble
    y <- getDouble
    z <- if hasZ then Just <$> getDouble else return Nothing
    m <- if hasM then Just <$> getDouble else return Nothing
    return $ Point x y z m

getHeader :: Get Header
getHeader = do
    or <- get	
    t <- fromIntegral <$>  getInt' or
    s <- if t .&. wkbSRID > 0 then (Just . fromIntegral) <$> getInt' or  else return Nothing 
    return $ Header or t s

-- number parsers
getNumber :: (Hexable a) => Int64 -> Endianness -> Get a
getNumber l end  = do
  bs <- getLazyByteString l
  case end of 
    BigEndian -> return $ fromHex bs
    LittleEndian -> return . fromHex . convertLittleEndian $ bs 

getByEnd :: Endianness -> (a -> a) -> (a -> a)
getByEnd e f = case e of 
  BigEndian -> id
  LittleEndian -> f

-- word32 = 4 bytes * 2 nibbles
getInt' :: Endianness -> Get Int
getInt' = getNumber 8

getInt :: Getter Int 
getInt = (getInt' <$> (asks _byteOrder)) >>= lift

-- word64 =  8 bytes * 2 nibbles
getDouble' :: Endianness -> Get Double 
getDouble' = getNumber 16

getDouble :: Getter Double
getDouble = (getDouble'  <$> (asks _byteOrder)) >>= lift
