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


makeHeader :: Int -> (Maybe Int) -> W.Header 
makeHeader t s = W.Header getSystemEndianness t s  

convertPointToWKB :: (Maybe Int) -> Point -> W.Point
convertPointToWKB s p = W.Point (makeHeader 1 s) p

convertLineStringToWKB :: (Maybe Int) -> LineString -> W.LineString
convertLineStringToWKB s (LineString v) = W.LineString (makeHeader 2 s) (V.length v) v  

convertLinearRingToWKB :: (Maybe Int) -> LinearRing -> W.LinearRing
convertLinearRingToWKB s (LinearRing v) = W.LinearRing (V.length v) v  

convertPolygonToWKB :: (Maybe Int) -> Polygon -> W.Polygon
convertPolygonToWKB s (Polygon v) = W.Polygon (makeHeader 3 s) (V.length v) $ V.map (convertLinearRingToWKB s) v 

convertMultiPointToWKB :: (Maybe Int) -> MultiPoint -> W.MultiPoint
convertMultiPointToWKB s (MultiPoint ps) = W.MultiPoint (makeHeader 4 s) (V.length ps) $ V.map (convertPointToWKB s) ps  


convertMultiLineStringToWKB :: (Maybe Int) -> MultiLineString -> W.MultiLineString
convertMultiLineStringToWKB s (MultiLineString ls) = W.MultiLineString  (makeHeader 5 s) (V.length ls) $ V.map (convertLineStringToWKB s) ls 

convertMultiPolygonToWKB :: (Maybe Int) -> MultiPolygon -> W.MultiPolygon
convertMultiPolygonToWKB s (MultiPolygon ps) = W.MultiPolygon (makeHeader 6 s) (V.length ps) $ V.map (convertPolygonToWKB s) ps

convertToWKB :: (Maybe Int) -> Geometry -> W.Geometry
convertToWKB s g = case g of
    PointGeometry p  ->  W.PointGeometry $ convertPointToWKB s p 
    LineStringGeometry v -> W.LineStringGeometry $ convertLineStringToWKB s v 
    PolygonGeometry v ->  W.PolygonGeometry $ convertPolygonToWKB s v 
    MultiPointGeometry mp -> W.MultiPointGeometry $ convertMultiPointToWKB s mp 
    MultiLineStringGeometry mls -> W.MultiLineStringGeometry $ convertMultiLineStringToWKB s mls 
    MultiPolygonGeometry mp -> W.MultiPolygonGeometry $ convertMultiPolygonToWKB s mp 

---

convertLinearRingFromWKB :: W.LinearRing -> LinearRing
convertLinearRingFromWKB (W.LinearRing i ps) = LinearRing ps 

convertFromWKB :: W.Geometry -> Feature
convertFromWKB g = case g of
  W.PointGeometry (W.Point h p) -> Feature  (W.srid h) (PointGeometry p)   
  W.LineStringGeometry (W.LineString h i ls) -> Feature (W.srid h) (LineStringGeometry $ LineString ls)
  W.PolygonGeometry (W.Polygon h i ls) -> Feature (W.srid h) (PolygonGeometry $ Polygon $ V.map convertLinearRingFromWKB ls) 
  {-W.MultiPointGeometry (W.MultiPoint h i ps) -> makeFeature h $ MultiPoint $ -}
  {-where-}
    {-makeFeature h geo = Feature (W.srid h) geo-}
