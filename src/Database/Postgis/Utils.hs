module Database.Postgis.Utils where
import qualified Database.Postgis.WKBTypes as W
import Database.Postgis.Types
import System.Endian


wkbZ =  0x80000000 :: Int
wkbM = 0x40000000 :: Int
wkbSRID = 0x20000000 :: Int
ewkbTypeOffset = 0x1fffffff :: Int
textHex = 0xC0000007 :: Int


convertToWKB :: Feature -> W.Geometry
convertToWKB (Feature s g) = 
  case g of
    Point x y m z = W.Point $ PointGeometry $ mkHeader  
    LinearRing v = 
    LineString v =
    Polygon v = 
    MultiPoint v = 
    MultiLineString v =
    MultiPolygon v = 
  where
    mkHeader t = Header getSystemEndianness t s  
     
{-convertFromWKB :: W.Geometry -> Feature-}
