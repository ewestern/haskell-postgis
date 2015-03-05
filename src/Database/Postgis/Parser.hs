{-# LANGUAGE OverloadedStrings, TemplateHaskell #-}

module Database.Postgis.Simple.Parser  where

import Database.Postgis.Utils
import Database.Postgis.WKBTypes
import qualified Database.Postgis.Geometry as G
--
import Data.Serialize
import Development.Placeholders
import qualified Data.ByteString as BS
import Data.ByteString.Lex.Integral
import Data.Bits
import qualified Data.Vector as V
import Control.Applicative
import Data.Binary.IEEE754
import System.Endian

class Hexable a where
  toHex :: a -> BS.ByteString
  fromHex :: BS.ByteString -> a

instance Hexable Int where
  toHex = toHexInt
  fromHex = fromHexInt

instance Hexable Double where
  fromHex = wordToDouble . fromHexInt 
  toHex = toHexInt . doubleToWord

 
instance Serialize Geometry where
  {-put p = $notImplemented-}
  put (PointGeometry p) = writePointGeometry p  
  put (LineStringGeometry ls) = writeLineString ls 
  put (PolygonGeometry p) = writePolygon p 
  put (MultiPointGeometry p) = writeMultiPoint p 
  put (MultiLineStringGeometry ls) = writeMultiLineString ls 
  put (MultiPolygonGeometry mp) = writeMultiPolygon mp 
     
-- todo: Validate geometry should compare header w/ geo characteristics
  get = do
    header <- get
    let tVal = (_geoType header) .&. ewkbTypeOffset
    case tVal of
      1 -> PointGeometry <$> parsePointGeometry header
      2 -> LineStringGeometry <$> parseLineString header
      3 -> PolygonGeometry <$> parsePolygon header
      4 -> MultiPointGeometry <$> parseMultiPoint header
      5 -> MultiLineStringGeometry <$> parseMultiLineString header
      6 -> MultiPolygonGeometry <$> parseMultiPolygon header
      {-7 -> parseGeoCollection header-}
      _ -> error "not yet implemented"


instance Serialize Header where 
  put (Header bo gt sr) = do
-- todo
    put bo
    writeNum bo gt 
    writeMaybeNum bo sr
  get = do
    or <- get	
    t <- parseNum or
    srid <- if t .&. wkbSRID > 0 then Just <$> parseNum or else return Nothing 
    return $ Header or t srid

instance Serialize Endianness where
  put BigEndian = putByteString $ toHex (0::Int)
  put LittleEndian = putByteString $ toHex (1::Int)
  get = do
    bs <- getByteString 2
    case fromHex bs :: Int of
      0 -> return BigEndian
      1 -> return LittleEndian
      _ -> error $ "not an endian: " ++ show bs

parsePoint :: Header -> Get G.Point
parsePoint (Header e gt sr) = do
	let hasM = if (gt .&. wkbM) > 0 then True else False 
	    hasZ = if (gt .&. wkbZ) > 0 then True else False
	x <- parseNum e
	y <- parseNum e
	m <- if hasM then Just <$> parseNum e else return Nothing
	z <- if hasZ then Just <$> parseNum e else return Nothing
	return $ G.Point x y m z

writePoint :: Header -> Putter G.Point 
writePoint (Header bo gt sr) (G.Point x y m z) = do
  writeNum bo x
  writeNum bo y
  writeMaybeNum bo m
  writeMaybeNum bo z
  

type LineSegment = (Int, V.Vector G.Point)

parseSegment :: Header ->  Get LineSegment
parseSegment head = do
  n <- parseNum $ _byteOrder head
  ps <- V.replicateM n $ parsePoint head
  return $ (n, ps)

parseRing :: Header -> Get LinearRing
parseRing head = do 
  (n, ps) <- parseSegment head
  return $ LinearRing n ps

writeRing :: Header -> Putter LinearRing
writeRing head (LinearRing n v) = do
  writeNum (_byteOrder head) n  
  V.mapM_ (writePoint head) v
  return ()
  
parsePointGeometry :: Header -> Get Point
parsePointGeometry head = do
	p <- parsePoint head
	return $ Point head p

writePointGeometry :: Putter Point
writePointGeometry (Point head p) = put head >> writePoint head p 
  
writeLineString :: Putter LineString
writeLineString (LineString head i v)  =  do
  put head
  writeNum (_byteOrder head) i
  V.mapM_ (writePoint head) v
  return ()

parseLineString :: Header -> Get LineString
parseLineString head = do
  (n, ps) <- parseSegment head
  return $ LineString head n ps


parseMulti ::  Serialize a => (Header -> Int -> V.Vector a -> b) -> Header -> Get b
parseMulti cons head = do
  n <- parseNum $ _byteOrder head
  ps <- V.replicateM n get
  return $ cons head n ps

parsePolygon :: Header -> Get Polygon
parsePolygon head = do
  n <- parseNum $ _byteOrder head
  vs <- V.replicateM n $ parseRing head  
  return $ Polygon head n vs 
 
writePolygon :: Putter Polygon
writePolygon (Polygon h i rs) = do
  put h
  writeNum (_byteOrder h) i
  V.mapM_ (writeRing h) rs
  return ()


writeMulti :: Header -> Int -> Putter (V.Vector Geometry) 
writeMulti head i g = do
  put head
  writeNum (_byteOrder head) i
  V.mapM_ put g

writeMultiPoint :: Putter MultiPoint
writeMultiPoint (MultiPoint h i g)  = writeMulti h i g

parseMultiPoint :: Header -> Get MultiPoint
parseMultiPoint = parseMulti MultiPoint

writeMultiLineString :: Putter MultiLineString
writeMultiLineString (MultiLineString h i g) = writeMulti h i g 

parseMultiLineString :: Header -> Get MultiLineString
parseMultiLineString = parseMulti MultiLineString
 
writeMultiPolygon :: Putter MultiPolygon
writeMultiPolygon (MultiPolygon h i g) = writeMulti h i g

parseMultiPolygon :: Header -> Get MultiPolygon
parseMultiPolygon = parseMulti MultiPolygon

parseNum :: (Num a, Hexable a) => Endianness -> Get a
parseNum BigEndian = fromHex <$> get  
parseNum LittleEndian = (fromHex . readLittleEndian) <$> get 

writeNum :: (Num a, Hexable a) => Endianness -> Putter a
writeNum BigEndian n = put $ toHex n
writeNum LittleEndian n = (put . readLittleEndian . toHex) n

writeMaybeNum :: (Num a, Hexable a) => Endianness -> Putter (Maybe a)
writeMaybeNum end (Just n)  = writeNum end n 
writeMaybeNum end Nothing = return () 

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
 
