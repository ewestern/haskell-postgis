module Database.Postgis.Serialize  where

import Database.Postgis.Utils
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

data Header = Header {
    _byteOrder :: Endianness
  , _geoType :: Int
} deriving (Show)

{-readGeometry :: BS.ByteString -> Geometry-}
{-readGeometry bs = case convertFromWKB . runGet . get $ bs of-}
       {-Left e -> error $ "Failed to parse geometry. " ++ e-}
       {-Right g -> g -}
  
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

makeHeader :: EWKBGeometry a => Geometry a -> Header 
makeHeader geo@(Geometry s g) = 
  let hasSRID = s /= Nothing
      gt = geoType geo
      wOr acc (p, h) = if p then h .|. acc else acc
      typ = foldl wOr gt [(hasM g,wkbM), (hasZ g, wkbZ), (s /= Nothing, wkbSRID)]   
  in Header getSystemEndianness typ 
 

class Writeable a where
  write :: Putter a 
 
instance Writeable Point2D where
  write p = writePoint p

instance Writeable Point3D where
  write p = writePoint p

instance Writeable Point4D where
  write p = writePoint p

instance Writeable LineString where
  write = writeLineString

instance Writeable Polygon where
  write = writePolygon   

instance Writeable MultiPoint where
  write = writeMultiPoint

instance Writeable MultiLineString where
  write = writeMultiLineString 
instance Writeable MultiPolygon where
  write = writeMultiPolygon

instance Writeable a => Serialize (Geometry a) where
  put geo@(Geometry s g) = do
    put $ makeHeader geo
    writeMaybeNum getSystemEndianness s
    write g
     
-- todo: Validate geometry should compare header w/ geo characteristics
  get = do
    header <- get
    s <- if (_geoType header) .&. wkbSRID > 0 
          then Just <$> parseNum' (_byteOrder header) 
          else return Nothing 
    let tVal = (_geoType header) .&. ewkbTypeOffset
        mkGeo p = do
          r <- runReaderT p header
          return $ Geometry s r
    case tVal of
      1 -> mkGeo parsePoint 
      2 -> mkGeo parseLineString 
      3 -> mkGeo parsePolygon 
      4 -> mkGeo parseMultiPoint 
      5 -> mkGeo parseMultiLineString 
      6 -> mkGeo parseMultiPolygon 
      {-7 -> parseGeoCollection header-}
      _ -> error "not yet implemented"


instance Serialize Header where 
  put (Header bo gt) = do
    put bo
    writeNum bo gt 
  get = do
    or <- get	
    t <- parseNum' or
    return $ Header or t 

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

type Parser = ReaderT Header Get

parseNum' :: (Num a, Hexable a) => Endianness -> Get a
parseNum' BigEndian =  fromHex <$> get
parseNum' LittleEndian = (fromHex . readLittleEndian) <$> get

parseNum :: (Num a, Hexable a) => Parser a
parseNum = do
  end <- asks _byteOrder
  lift $ parseNum' end 

makePointParser :: Point a => Bool -> Bool -> Parser a 
makePointParser hasM hasZ 
  | hasM && hasZ = parsePoint4  
  | not hasM && hasZ = parsePoint3Z 
  | hasM && not hasZ = parsePoint3M 
  | not hasM && not hasZ = parsePoint2 

parsePoint :: Point a => Parser a 
parsePoint = do
    gt <- asks _geoType 
    let hasM = if (gt .&. wkbM) > 0 then True else False 
        hasZ = if (gt .&. wkbZ) > 0 then True else False
    makePointParser hasM hasZ

parsePoint2 :: Parser Point2D
parsePoint2 =  Point2D <$> parseNum <*> parseNum

parsePoint3M :: Parser Point3D
parsePoint3M = Point3DM <$> parseNum <*> parseNum <*> parseNum

parsePoint3Z :: Parser Point3D
parsePoint3Z = Point3DZ <$> parseNum <*> parseNum <*> parseNum

parsePoint4 :: Parser Point4D
parsePoint4 = Point4D <$> parseNum <*> parseNum <*> parseNum <*> parseNum

parseSegment :: Point a => Parser (V.Vector a )
parseSegment = parseNum >>= (\n -> V.replicateM n parsePoint) 
  
parseRing :: Parser LinearRing
parseRing = LinearRing <$> parseSegment 

parseLineString :: Parser LineString
parseLineString = LineString <$> parseSegment

parsePolygon :: Parser Polygon
parsePolygon = Polygon <$> (parseNum >>= (\n -> V.replicateM n parseRing))  
 
parseMulti :: Serialize a => Parser (V.Vector a)
parseMulti = parseNum >>= (\n -> V.replicateM n (lift get))
{-parseMulti = do-}
  {-n <- parseNum-}
  {-v <- V.replicateM n $ lift get -}
  {-return v-}

parseMultiPoint :: Parser MultiPoint
parseMultiPoint = MultiPoint <$> parseMulti 

parseMultiLineString :: Parser MultiLineString
parseMultiLineString = MultiLineString <$> parseMulti 

parseMultiPolygon :: Parser  MultiPolygon
parseMultiPolygon = MultiPolygon <$> parseMulti 


--writers
writeNum :: (Num a, Hexable a) => Endianness -> Putter a
writeNum BigEndian n = put $ toHex n
writeNum LittleEndian n = (put . readLittleEndian . toHex) n

writeMaybeNum :: (Num a, Hexable a) => Endianness -> Putter (Maybe a)
writeMaybeNum end (Just n)  = writeNum end n 
writeMaybeNum end Nothing = return () 


writePoint :: Point a => Putter a
writePoint p = do
  let bo = getSystemEndianness
  writeNum bo $ _x p
  writeNum bo $ _y p
  writeMaybeNum bo $ _m p
  writeMaybeNum bo $ _z p

  
writeLineString :: Putter LineString
writeLineString (LineString v) = do
  writeNum getSystemEndianness $ V.length v
  V.mapM_ writePoint v
  return ()

writeRing :: Putter LinearRing
writeRing (LinearRing v) = do
  writeNum getSystemEndianness $ V.length v  
  V.mapM_ writePoint v
  return ()
 
writePolygon :: Putter Polygon
writePolygon (Polygon rs) = do
  writeNum getSystemEndianness $ V.length rs 
  V.mapM_ writeRing rs
  return ()


writeMulti :: Serialize a => Putter (V.Vector a)
writeMulti v = do
  writeNum getSystemEndianness $ V.length v 
  V.mapM_ put v 
  return ()

writeMultiPoint :: Putter MultiPoint
writeMultiPoint (MultiPoint ps) = writeMulti ps  

writeMultiLineString :: Putter MultiLineString
writeMultiLineString (MultiLineString ls) = writeMulti ls    

writeMultiPolygon :: Putter MultiPolygon
writeMultiPolygon (MultiPolygon ps) = writeMulti ps 
