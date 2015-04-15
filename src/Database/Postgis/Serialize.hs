{-# LANGUAGE GADTs, FlexibleInstances, OverloadedStrings #-}

module Database.Postgis.Serialize  where

import Database.Postgis.Utils
import Database.Postgis.Geometry
{-import Data.Serialize -}
import Data.Binary
import Data.Word
import Development.Placeholders
import Data.ByteString.Lex.Integral
import Data.Bits
import Control.Applicative ((<*>), (<$>))
import Data.Binary.IEEE754
import System.Endian
import Control.Monad.Reader
import Data.Int

import qualified Data.Vector as V
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BL
import Data.ByteString.Builder


{-import Data.Serialize.Builder -}
import Data.Typeable


{-The "hex" format encodes binary data as 2 hexadecimal digits per byte, most significant nibble first. The entire string is preceded by the sequence \x (to distinguish it from the escape format). In some contexts, the initial backslash may need to be escaped by doubling it, in the same cases in which backslashes have to be doubled in escape format; details appear below. The hexadecimal digits can be either upper or lower case, and whitespace is permitted between digit pairs (but not within a digit pair nor in the starting \x sequence). The hex format is compatible with a wide range of external applications and protocols, and it tends to be faster to convert than the escape format, so its use is preferred.-}


{-V.replicateM :: Monad m => Word32 -> m a -> m (V.Vector a )-}
{-V.replicateM = V.replicateM . fromIntegral-}

readGeometry :: BS.ByteString -> Geometry 
{-readGeometry bs = error $ show bs-}
readGeometry bs = case runGet parseGeometry bs of
           Left e -> error $ "failed parse" ++ e
           Right g -> g 

writeGeometry :: Geometry -> BS.ByteString
writeGeometry = runPut . putGeometry
{-writeGeometry = toByteString . execPut . putGeometry-}

data Header = Header {
    _byteOrder :: Endianness
  , _geoType :: Word32
  , _srid :: SRID
} deriving (Show)


class Hexable a where
  {-toHex :: a -> Builder-}
  toHex :: a -> BS.ByteString
  fromHex :: BS.ByteString -> a

instance Hexable Int where
  toHex = toHexWord32 . fromIntegral
  fromHex = fromHexInt

instance Hexable Word8 where
  toHex = toHexInt8  . fromIntegral
  fromHex = fromHexInt

instance Hexable Word32  where
  toHex =  toHexWord32
  fromHex = fromHexInt

instance Hexable Double where
  fromHex = wordToDouble . fromHexInt 
  toHex = toHexDouble 
  {-toHex = toHexWord64 . doubleToWord-}

makeHeader :: EWKBGeometry a => SRID -> a -> Header
makeHeader s geo =
  let gt = geoType geo
      wOr acc (p, h) = if p then h .|. acc else acc
      typ = foldl wOr gt [(hasM geo, wkbM), (hasZ geo, wkbZ), (s /= Nothing, wkbSRID)]   
  in Header getSystemEndianness typ s 


instance Serialize Geometry where
  get = parseGeometry
  put = putGeometry

parseGeometry :: Get Geometry 
parseGeometry = do
    h <- lookAhead get
    let tVal = (_geoType h) .&. ewkbTypeOffset
    case tVal of
      1 -> mkGeo h GeoPoint parseGeoPoint
      2 -> mkGeo h GeoLineString parseLineString 
      3 -> mkGeo h GeoPolygon parsePolygon 
      4 -> mkGeo h GeoMultiPoint parseMultiPoint 
      5 -> mkGeo h GeoMultiLineString parseMultiLineString
      6 -> mkGeo h GeoMultiPolygon  parseMultiPolygon 
      _ -> error $ "not yet implemented" ++ (show tVal)

mkGeo :: Header -> (SRID -> a -> Geometry) -> Parser a -> Get Geometry
mkGeo h cons p = (cons (_srid h)) <$> runReaderT p h

instance Serialize Header where 
  put (Header bo gt s) = put bo >> writeNum gt  >> writeMaybeNum s
  get = parseHeader

parseHeader :: Get Header  
parseHeader = do 
    or <- get	
    t <- fromIntegral <$>  parseInt' or
    s <- if t .&. wkbSRID > 0 then (Just . fromIntegral) <$> parseInt' or  else return Nothing 
    return $ Header or t s


type Parser = ReaderT Header Get

parseNumber :: (Hexable a , Num a) => Int -> Endianness -> Get a
parseNumber l end = do
  bs <- getByteString l
  case end of 
    BigEndian -> return $ fromHex bs
    LittleEndian -> return . fromHex . convertLittleEndian $ bs 

parseInt' :: Endianness -> Get Int 
parseInt' = parseNumber 8

parseInt :: Parser Int 
parseInt = (parseInt' <$> (asks _byteOrder)) >>= lift

parseDouble' :: Endianness -> Get Double 
parseDouble' = parseNumber 16

parseDouble :: Parser Double
parseDouble = (parseDouble' <$> (asks _byteOrder)) >>= lift

instance Serialize Endianness where
  put BigEndian = put $ toHex $ (0 :: Int) 
  put LittleEndian = put $ toHex $ (1 :: Int) 
  get = do
    bs <- getByteString 2
    case (fromHex bs) :: Word8 of
      0 -> return BigEndian
      1 -> return LittleEndian
      _ -> error $ "not an endian: " ++ show bs 


--


{-parseNum' :: (Show a, Num a, Hexable a) => Endianness -> Get a-}
{-parseNum' BigEndian =  fromHex <$> get-}
{-parseNum' LittleEndian = (fromHex . convertLittleEndian) <$> get-}

{-parseNum :: (Show a, Num a, Hexable a) => Parser a-}
{-parseNum = do-}
  {-end <- asks _byteOrder-}
  {-lift $ parseNum' end -}

parseGeoPoint :: Parser Point
parseGeoPoint = lift parseHeader >> parsePoint

parsePoint :: Parser Point
parsePoint = do
    gt <- asks _geoType 
    let hasM = if (gt .&. wkbM) > 0 then True else False 
        hasZ = if (gt .&. wkbZ) > 0 then True else False
    x <- parseDouble
    y <- parseDouble
    z <- if hasZ then Just <$> parseDouble else return Nothing
    m <- if hasM then Just <$> parseDouble else return Nothing
    return $ Point x y z m

parseSegment :: Parser (V.Vector Point)
parseSegment = parseInt >>= (\n -> V.replicateM n parsePoint) 
  
parseRing :: Parser LinearRing
parseRing = parseSegment 

parseLineString :: Parser LineString 
parseLineString = lift parseHeader >> LineString <$> parseSegment

parsePolygon :: Parser Polygon 
parsePolygon = lift parseHeader >> Polygon <$> (parseInt >>= (\n -> V.replicateM n parseRing))

parseMultiPoint :: Parser MultiPoint 
parseMultiPoint = do
  lift parseHeader
  n <- parseInt 
  ps <- V.replicateM n parseGeoPoint
  return $ MultiPoint ps

parseMultiLineString :: Parser MultiLineString 
parseMultiLineString = do
  lift parseHeader
  n <- parseInt
  ls <- V.replicateM n parseLineString 
  return $ MultiLineString ls

parseMultiPolygon :: Parser MultiPolygon 
parseMultiPolygon = do 
  lift parseHeader
  n <- parseInt
  ps <- V.replicateM n parsePolygon
  return $ MultiPolygon ps


-- writers

writeNum :: (Hexable a, Num a) => Putter a
writeNum = put . toHex

writeMaybeNum :: (Num a, Hexable a) => Putter (Maybe a)
writeMaybeNum (Just n)  = writeNum n 
writeMaybeNum Nothing = return () 

writePoint :: Putter Point 
writePoint p = do 
{-writePoint p = do-}
  writeNum  $ _x p
  writeNum  $ _y p
  {-writeMaybeNum  $ _m p-}
  {-writeMaybeNum  $ _z p-}

writeRing :: Putter LinearRing
writeRing v = do
  writeNum . V.length $ v  
  V.mapM_ writePoint v
  return ()


putGeometry :: Putter Geometry
putGeometry (GeoPoint s p) = do
  put $ makeHeader s p
  writePoint p
  return ()

putGeometry (GeoLineString s ls@(LineString v)) = do
  put $ makeHeader s ls 
  writeNum . V.length $ v
  V.mapM_ writePoint v
  return ()

putGeometry (GeoPolygon s pg@(Polygon rs)) = do
  put $ makeHeader s pg
  writeNum . V.length $ rs 
  V.mapM_ writeRing rs
  return ()

putGeometry (GeoMultiPoint s mp@(MultiPoint ps)) = do
  put $ makeHeader s mp
  writeNum . V.length $ ps 
  V.mapM_ (putGeometry . GeoPoint s)  ps
  return ()

putGeometry (GeoMultiLineString s mls@(MultiLineString ls)) = do
  put $ makeHeader s mls
  writeNum . V.length $ ls 
  V.mapM_ (putGeometry . GeoLineString s) ls
  return ()

putGeometry (GeoMultiPolygon s mpg@(MultiPolygon ps)) = do
  put $ makeHeader s mpg
  writeNum . V.length $  ps 
  V.mapM_ (putGeometry . GeoPolygon s)  ps
  return ()

