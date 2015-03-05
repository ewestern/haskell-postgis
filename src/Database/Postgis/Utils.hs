module Database.Postgis.Utils where
import qualified Database.Postgis.WKBTypes as W
import Database.Postgis.Geometry
import qualified Data.Vector as V
import System.Endian


wkbZ =  0x80000000 :: Int
wkbM = 0x40000000 :: Int
wkbSRID = 0x20000000 :: Int
ewkbTypeOffset = 0x1fffffff :: Int
textHex = 0xC0000007 :: Int



convertToWKB :: Feature -> W.Geometry
convertToWKB (Feature s g) = case g of
    PointGeometry p  ->  W.PointGeometry $ W.Point (mkHeader 1) p 
    LineString v = W.LineStringGeometry $ W.LineString (mkHeader 2) (V.length) v
    Polygon v =  W.PolygonGeometry $ W.Polygon (mkHeader 3) $ V.map convertLinearRing v
    MultiPoint vp = W.MultiPointGeometry $ W.MultiPoint (mkHeader 4) 
    {-MultiPoint v = -}
    {-MultiLineString v =-}
    {-MultiPolygon v = -}
  where
    mkHeader t = W.Header getSystemEndianness t s  
    convertLinearRing (LinearRing v) = W.LinearRing (V.length v) v 
     
{-convertFromWKB :: W.Geometry -> Feature-}
