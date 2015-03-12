{-# LANGUAGE OverloadedStrings, TemplateHaskell #-}

module Database.Postgis.Serialize  where

import Database.Postgis.Utils
import Database.Postgis.WKBTypes
import Database.Postgis.Geometry
--
import Data.Serialize
import Data.Serialize.Get
import Development.Placeholders
import qualified Data.ByteString as BS
import Data.ByteString.Lex.Integral
import Data.Bits
import qualified Data.Vector as V
import Control.Applicative ((<*>), (<$>))
import Data.Binary.IEEE754
import System.Endian

import Control.Monad.Reader

readGeometry :: BS.ByteString -> Geometry
readGeometry bs = case convertFromWKB . runGet . get $ bs of
       Left e -> error $ "Failed to parse geometry. " ++ e
       Right g -> g 
  
{-writeGeometry :: G.Geometry -> BS.ByteString-}
{-writeGeometry g =  -}

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
  put g = do
-- todo: need to construct geotype from constituant 
    case geometry g of
      Point p ->  writePointGeometry p 
      LineString ps -> writeLineString ps
      Polygon ls ->  writePolygon ls 
      MultiPoint mp ->  w
      


      
-- todo: Validate geometry should compare header w/ geo characteristics
  get = do
    header <- get
    let tVal = (geoType header) .&. ewkbTypeOffset
    case tVal of
      1 -> Geometry <$> runReaderT parsePointGeometry header
      2 -> Geometry <$> runReaderT parseLineString header
      3 -> Geometry <$> runReaderT parsePolygon header
      4 -> Geometry <$> runReaderT parseMultiPoint header
      5 -> Geometry <$> runReaderT parseMultiLineString header
      6 -> Geometry <$> runReaderT parseMultiPolygon header
      {-7 -> parseGeoCollection header-}
      _ -> error "not yet implemented"


instance Serialize Header where 
  put (Header bo gt sr) = do
    put bo
    writeNum bo gt 
    writeMaybeNum bo sr
  get = do
    or <- get	
    t <- parseNum' or
    srid <- if t .&. wkbSRID > 0 then Just <$> parseNum' or else return Nothing 
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


--readers

type LineSegment = V.Vector Point
type Parser = ReaderT Header Get

parseNum' :: (Num a, Hexable a) => Endianness -> Get a
parseNum' BigEndian =  fromHex <$> get
parseNum' LittleEndian = (fromHex . readLittleEndian) <$> get

parseNum :: (Num a, Hexable a) => Parser a
parseNum = do
  end <- asks byteOrder
  lift $ parseNum' end 

parsePoint :: Parser Point
parsePoint = do
    gt <- asks geoType 
    let hasM = if (gt .&. wkbM) > 0 then True else False 
        hasZ = if (gt .&. wkbZ) > 0 then True else False
    x <- parseNum
    y <- parseNum
    m <- if hasM then Just <$> parseNum else return Nothing
    z <- if hasZ then Just <$> parseNum else return Nothing
    return  $ Point x y m z


parseSegment :: Parser LineSegment
parseSegment = parseNum >>= (\n -> V.replicateM n parsePoint) 
  {-n <- parseNum-}
  {-ps <- V.replicateM n parsePoint-}
  {-return $ (n, ps) -}
  
parseRing :: Parser LinearRing
parseRing = LinearRing <$> parseSegment 

{-parsePointGeometry :: Parser Point-}
{-parsePointGeometry = Point <$> ask <*> parsePoint -}

parseLineString :: Parser LineString
parseLineString = LineString <$> parseSegment
 {-do-}
  {-head <- ask-}
  {-(n, ps) <- parseSegment-}
  {-return $ LineString head n ps-}


parsePolygon :: Parser Polygon
parsePolygon = Polygon <$> (parseNum >>= (\n -> V.replicateM n parseRing))  
 
{-parseMulti :: (Header -> Int -> V.Vector a -> b) -> Parser a -> Parser b-}
{-parseMulti cons p = do-}
  {-h <- ask-}
  {-n <- parseNum-}
  {-ps <- V.replicateM n p-}
  {-return $ cons h n ps-}

{-parseMultiPoint :: Parser MultiPoint-}
{-parseMultiPoint = MultiPoint <$> (parseNum >>= (\n -> V.replicateM n parseGeometry)) -}
{-parseMulti MultiPoint parsePointGeometry -}

{-parseMultiLineString :: Parser MultiLineString-}
{-parseMultiLineString = parseMulti MultiLineString parseLineString -}

{-parseMultiPolygon :: Parser  MultiPolygon-}
{-parseMultiPolygon = parseMulti MultiPolygon parsePolygon-}


--writers
writeNum :: (Num a, Hexable a) => Endianness -> Putter a
writeNum BigEndian n = put $ toHex n
writeNum LittleEndian n = (put . readLittleEndian . toHex) n

writeMaybeNum :: (Num a, Hexable a) => Endianness -> Putter (Maybe a)
writeMaybeNum end (Just n)  = writeNum end n 
writeMaybeNum end Nothing = return () 


writePoint :: Putter Point 
writePoint (Point x y m z) = do
  let bo = getSystemEndianness
  writeNum bo x
  writeNum bo y
  writeMaybeNum bo m
  writeMaybeNum bo z

writeRing :: Putter LinearRing
writeRing (LinearRing n v) = do
  writeNum getSystemEndianness n  
  V.mapM_ writePoint v
  return ()
 
writeGeometry :: Putter (Geometry a)
writeGeometry 

makeHeader :: Geometry a -> Put ()
makeHeader

writePointGeometry :: Putter Point
writePointGeometry (Point p) = writePoint p 
  
writeLineString :: Putter LineString
writeLineString (LineString v) = do
  writeNum getSystemEndianness $ V.length v
  V.mapM_ (writePoint head) v
  return ()

 
-- todo, this should not take an endian
writePolygon :: Putter Polygon
writePolygon (Polygon rs) = do
  writeNum getSystemEndianness $ V.length v
  V.mapM_ writeRing rs
  return ()

writeMultiPoint :: Putter MultiPoint
writeMultiPoint (MultiPoint ps) = do
  writeNum getSystemEndianness $ V.length ps 
  V.mapM_ (put . Geometry) ps 
  return ()

writeMultiLineString :: Putter MultiLineString
writeMultiLineString  

{-writeMulti :: Serialize a => Header -> Int -> Putter (V.Vector a) -}
{-writeMulti head i g = do-}
  {-put head-}
  {-writeNum (byteOrder head) i-}
  {-V.mapM_ put g-}

{-writeMultiPoint :: Putter MultiPoint-}
{-writeMultiPoint (MultiPoint h i g)  = writeMulti h i (PointGeometry <$> g)-}

{-writeMultiLineString :: Putter MultiLineString-}
{-writeMultiLineString (MultiLineString h i g) = writeMulti h i (LineStringGeometry <$> g) -}


{-writeMultiPolygon :: Putter MultiPolygon-}
{-writeMultiPolygon (MultiPolygon h i g) = writeMulti h i (PolygonGeometry <$> g)-}

-- Util
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
 
