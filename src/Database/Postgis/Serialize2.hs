{-# LANGUAGE TypeFamilies, GADTs, FlexibleInstances, OverloadedStrings #-}
import Database.Postgis.Geometry
import Database.Postgis.Utils

import Data.Bits
import Control.Monad.Reader
import qualified Data.ByteString.Builder as B
import qualified Data.ByteString.Lazy as BL
import Control.Applicative ((<*>), (<$>))
import Control.Monad.Writer.Lazy
import Data.Binary.Get
import System.Endian
import qualified Data.Vector as V

import Data.Binary.IEEE754
import Data.Word
import System.Endian
import Control.Monad.Identity (Identity)


  
writeGeometry :: Geometry -> BL.ByteString
writeGeometry = B.toLazyByteString . execWriter . putGeometry 

{-readGeometry :: BL.ByteString -> Geometry-}
{-readGeometry = runGet getGeometry-}

type EndGet = ReaderT Header Get 

type Put = WriterT B.Builder Identity

type Putter a = a -> Put ()

class Serialize a where
  put :: Putter a
  get :: Get a 

instance Serialize Endianness where
  get = fromHex <$> (getByteString 2) 
  put = tell . toHex

instance Serialize Header where
  get = getHeader 
  put (Header bo gt s) = put bo >> putInt32 gt  >> putMaybe s putInt32

class Hexable a where
  toHex :: a -> B.Builder
  fromHex :: BL.ByteString -> a

instance Hexable Endianness where
  toHex BigEndian = B.int8HexFixed 0  
  toHex LittleEndian = B.int8HexFixed 1  
  fromHex b = case (fromHexInt b) :: Word8 of
    0 -> BigEndian
    1 -> LittleEndian
    _ -> error $ "Not an Endian" ++ show b

instance Hexable Int where
  toHex = B.int32HexFixed . fromIntegral
  fromHex = fromHexInt

instance Hexable Word32  where
  toHex =  B.int32HexFixed . fromIntegral
  fromHex = fromHexInt

instance Hexable Double where
  fromHex = wordToDouble . fromHexInt 
  toHex = B.doubleHexFixed 

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
  putInt32 . V.length $ v  
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
  putInt32 . V.length $ rs 
  V.mapM_ putRing rs

putGeometry (GeoMultiPoint s mp@(MultiPoint ps)) = do
  put $ makeHeader s mp
  putInt32 . V.length $ ps 
  V.mapM_ (putGeometry . GeoPoint s)  ps

putGeometry (GeoMultiLineString s mls@(MultiLineString ls)) = do
  put $ makeHeader s mls
  putInt32 . V.length $ ls 
  V.mapM_ (putGeometry . GeoLineString s) ls

putGeometry (GeoMultiPolygon s mpg@(MultiPolygon ps)) = do
  put $ makeHeader s mpg
  putInt32 . V.length $  ps 
  V.mapM_ (putGeometry . GeoPolygon s)  ps

----
putPoint :: Putter Point
putPoint (Point x y m z) = putDouble x >> putDouble y >> putMaybe m putDouble >> putMaybe z putDouble

putDouble :: Putter Double
putDouble = tell . toHex

putInt32 :: Integral a => Putter a
putInt32 = tell . B.int32HexFixed . fromIntegral 

putMaybe ::  Maybe a -> Putter a -> Put ()
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
      mkGeo :: (SRID -> a -> Geometry) -> EndGet a -> Get Geometry
      mkGeo cons p = cons (_srid h) <$> runReaderT p h
  case t of
      1 -> mkGeo GeoPoint getGeoPoint
      2 -> mkGeo GeoLineString getLineString 
      3 -> mkGeo GeoPolygon getPolygon 
      4 -> mkGeo GeoMultiPoint getMultiPoint 
      5 -> mkGeo GeoMultiLineString getMultiLineString
      6 -> mkGeo GeoMultiPolygon  getMultiPolygon 
      _ -> error $ "not yet implemented" ++ (show t)


getMultiPolygon :: EndGet MultiPolygon 
getMultiPolygon = do 
  lift getHeader
  n <- getInt
  ps <- V.replicateM n getPolygon
  return $ MultiPolygon ps

getMultiLineString :: EndGet MultiLineString 
getMultiLineString = do
  lift getHeader
  n <- getInt
  ls <- V.replicateM n getLineString 
  return $ MultiLineString ls

getMultiPoint :: EndGet MultiPoint 
getMultiPoint = do
  lift getHeader
  n <- getInt 
  ps <- V.replicateM n getGeoPoint
  return $ MultiPoint ps
 
getPolygon :: EndGet Polygon 
getPolygon = lift getHeader >> Polygon <$> (getInt >>= (\n -> V.replicateM n getRing))

getLineString :: EndGet LineString 
getLineString = lift getHeader >> LineString <$> getSegment

getRing :: EndGet LinearRing
getRing = getSegment 

getSegment :: EndGet (V.Vector Point)
getSegment = getInt >>= (\n -> V.replicateM n getPoint) 
 
getGeoPoint :: EndGet Point
getGeoPoint = lift getHeader >> getPoint

getPoint :: EndGet Point
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

getNumber :: (Hexable a) => Int -> Endianness -> Get a
getNumber l end  = do
  bs <- getByteString l
  case end of 
    BigEndian -> return $ fromHex bs
    LittleEndian -> return . fromHex . convertLittleEndian $ bs 

-- word32 = 4 bytes * 2 nibbles
getInt' :: Endianness -> Get Int 
getInt' = getNumber 8

getInt :: EndGet Int 
getInt = (getInt' <$> (asks _byteOrder)) >>= lift

-- word64 =  8 bytes * 2 nibbles
getDouble' :: Endianness -> Get Double 
getDouble' = getNumber 16

getDouble :: EndGet Double
getDouble = (getDouble'  <$> (asks _byteOrder)) >>= lift
