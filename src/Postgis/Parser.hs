module Database.Postgis.Simple.Parser  (
  parseGeometry
) where
import Database.Postgis.Simple.Utils
import Data.Serialize.Put
import Postgis.Types
import qualified Data.ByteString as BS
import Data.Bits
import qualified Data.Vector as V
import Data.Text.Read
import Control.Applicative
import Data.Text.Encoding
import Data.Binary.IEEE754

parseGeometry :: Get Geometry
parseGeometry = do
  header <- parseHeader
  let tVal = (_geoType header) .&. ewkbTypeOffset
  case tVal of
    1 -> Point <$> parsePointGeometry header
    2 -> LineString <$> parseLineString header
    3 -> Polygon <$> parsePolygon header
    4 -> MultiPoint <$> parseMultiPoint header
    5 -> MultiLineString <$> parseMultiLineString header
    6 -> MultiPolygon <$> parseMultiPolygon header
    {-7 -> parseGeoCollection header-}
    _ -> error "not yet implemented"

parsePoint :: Header -> Get WKBPoint
parsePoint header = do
	let hasM = if (_geoType header .&. wkbM) > 0 then True else False 
	    hasZ = if (_geoType header .&. wkbZ) > 0 then True else False
	    e = _byteOrder header
	x <- parseDouble e
	y <- parseDouble e
	m <- if hasM then Just <$> parseDouble e else return Nothing
	z <- if hasZ then Just <$> parseDouble e else return Nothing
	return $ WKBPoint x y m z

parseSegment :: Header ->  Get LineSegment
parseSegment head  = do
  n <- parseInt $ _byteOrder head
  ps <- V.replicateM n $ parsePoint head
  return $ (n, ps)

parseRing :: Header -> Get LinearRing
parseRing head = (LinearRing <$> fst <*> snd) <$> parseSegment head
  
parseHeader :: Get Header
parseHeader = do
	or <- parseEndian	
	t <- parseInt or
	srid <- if t .&. wkbSRID > 0 then Just <$> parseInt or else return Nothing
	return $ Header or t srid

parsePointGeometry :: Header -> Get PointGeometry
parsePointGeometry head = do
	p <- parsePoint head
	return $ PointGeometry head p

parseLineString :: Header -> Get LineStringGeometry
parseLineString head = ((LineStringGeometry head) <$> fst <*> snd) <$> parseSegment head

parsePolygon :: Header -> Get PolygonGeometry
parsePolygon head = do
  n <- parseInt $ _byteOrder head
  vs <- V.replicateM n $ parseRing head  
  return $ PolygonGeometry head n vs 
 
parseMultiPoint :: Header -> Get MultiPointGeometry
parseMultiPoint head = do
  n <- parseInt $ _byteOrder head
  ps <- V.replicateM n $ parseGeometry
  return $ MultiPointGeometry n ps

parseMultiLineString :: Header -> Get MultiLineStringGeometry
parseMultiLineString head = do
  n <- parseInt $ _byteOrder head
  ps <- V.replicateM n $ parseGeometry
  return $ MultiLineStringGeometry n ps
  
parseMultiPolygon :: Header -> Get MultiPolygonGeometry
parseMultiPolygon head = do
  n <- parseInt $ _byteOrder head
  ps <- V.replicateM n $ parseGeometry
  return $ MultiPolygonGeometry n ps


parseEndian :: Get Endian
parseEndian = do
  bs <- getByteString 2
  case parseHex bs :: Int of
    0 -> return BigEndian
    1 -> return LittleEndian
    _ -> error $ "not an endian: " ++ show bs

parseDouble :: Endian -> Get Double
parseDouble end = do
  bs <- getByteString 16
  case end of
    BigEndian -> return $ getDouble bs
    LittleEndian -> return $ getDouble $ readEndian bs  
    where
      getDouble = wordToDouble . parseHex

parseInt :: Endian -> Get Int
parseInt end = do
  bs <- getByteString 8
  case end of 
      BigEndian ->  return $ parseHex bs
      LittleEndian ->  return $ parseHex $ readEndian bs 

parseHex :: Integral a => BS.ByteString -> a
parseHex bs = case hexadecimal . decodeUtf8 $ bs of
      Right (v, r) ->  v
      Left s -> error s 

readEndian :: BS.ByteString -> BS.ByteString
readEndian bs = BS.concat . reverse $ splitEvery bs
  where
    splitEvery bs = 
      let (first, rest) = BS.splitAt 2 bs in 
      if BS.null bs then [] else first : (splitEvery rest)
 
