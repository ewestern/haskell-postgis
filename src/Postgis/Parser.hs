module Postgis.Parser  (
  parseGeometry
) where
import Data.Serialize.Get
import Postgis.Types
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as C
import Data.Bits
import qualified Data.Vector as V
import Data.Text.Read
import Control.Applicative
import Data.Text.Encoding
import Data.Binary.IEEE754

wkbZ =  0x80000000
wkbM = 0x40000000
wkbSRID = 0x20000000
ewkbTypeOffset = 0x1fffffff

textHex = 0xC0000007

parsePoint :: Header -> Get Point
parsePoint header = do
	let hasM = if (_geoType header .&. wkbM) > 0
              then True
              else False 
	    hasZ = if (_geoType header .&. wkbZ) > 0
              then True
              else False
	    e = _byteOrder header
	x <- parseDouble e
	y <- parseDouble e
	m <- if hasM then Just <$> parseDouble e else return Nothing
	z <- if hasZ then Just <$> parseDouble e else return Nothing
	return $ WKBPoint x y m z

parseHeader :: Get Header
parseHeader = do
	or <- parseEndian	
	t <- parseInt or
	srid <- if t .&. wkbSRID > 0 then Just <$> parseInt or else return Nothing
	return $ Header or t srid

parsePointGeometry :: Header -> Get Geometry
parsePointGeometry head = do
	p <- parsePoint head
	return $ Point $ PointGeometry head p

parseLineString :: Header -> Get Geometry
parseLineString head = do
        n <- parseInt $ _byteOrder head
        ps <- V.replicateM n $ parsePoint head
        return $ LineString $ LineStringGeometry head n ps	

parseGeometry = do
	header <- parseHeader
	let tVal = (_geoType header) .&. ewkbTypeOffset
	case tVal of
		1 -> parsePointGeometry	header
		2 -> parseLineString header
		_ -> error "not yet implemented"

-- use Data.Text.Read (hexadecimal) to read a hex string into a Word (32 for integer 64 for double)

parseEndian :: Get Endian
parseEndian = do
  bs <- getByteString 4
  -- drop initial / in endian definition
  case parseHex (BS.drop 2 bs) :: Int of
    0 -> return BigEndian
    1 -> return LittleEndian


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
    splitEvery bs = let (first, rest) = BS.splitAt 2 bs in 
      if BS.null bs
         then []
          else first : (splitEvery rest)
 
