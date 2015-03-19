{-# LANGUAGE ExistentialQuantification, FlexibleInstances, GADTs #-}

module Database.Postgis.Geometry where

import qualified Data.Vector as V
import Development.Placeholders
import qualified Data.Text as T

type SRID = Maybe Int

class EWKBGeometry a where
  hasM :: a -> Bool
  hasZ :: a -> Bool
  geoType :: a -> Int

  
data Point = Point {
    _x :: {-# UNPACK #-} !Double
  , _y :: {-# UNPACK #-} !Double
  , _z :: Maybe Double
  , _m :: Maybe Double
} deriving (Show)

instance EWKBGeometry Point where
  hasM (Point x y z m) = m /= Nothing 
  hasZ (Point x y z m) = z /= Nothing 
  geoType _ = 1
 


-- todo, would like to dependently type this
data LinearRing =  LinearRing (V.Vector Point) 

instance Show LinearRing where
  show (LinearRing vs) = show vs


{-instance EWKBGeometry LinearRing where-}
  {-hasM (LinearRing ps) = hasM . V.head $ ps-}
  {-hasZ (LinearRing ps) = hasZ . V.head $ ps-}

data LineString = LineString (V.Vector Point) 

instance Show LineString where
  show (LineString vs) = show vs

instance EWKBGeometry LineString where
  hasM (LineString ps) = hasM . V.head $ ps
  hasZ (LineString ps) = hasZ . V.head $ ps
  geoType _ = 2

data Polygon = Polygon (V.Vector LinearRing) deriving (Show)

hasMLinearRing :: LinearRing -> Bool
hasMLinearRing (LinearRing ps) = hasM . V.head $ ps

hasZLinearRing :: LinearRing -> Bool
hasZLinearRing (LinearRing ps) = hasZ . V.head $ ps

instance EWKBGeometry Polygon where
  hasM (Polygon ps) = hasMLinearRing . V.head $ ps
  hasZ (Polygon ps) = hasZLinearRing . V.head $ ps
  geoType _ = 3

data MultiPoint = MultiPoint (V.Vector (Geometry Point)) 

instance Show MultiPoint where
  show (MultiPoint ps) = show ps

instance EWKBGeometry MultiPoint where
  hasM (MultiPoint ps) = hasM . V.head $ ps
  hasZ (MultiPoint ps) = hasZ . V.head $ ps
  geoType _ = 4

data MultiLineString = MultiLineString (V.Vector (Geometry LineString)) deriving (Show)

instance EWKBGeometry MultiLineString where
  hasM (MultiLineString ps) = hasM . V.head $ ps
  hasZ (MultiLineString ps) = hasZ . V.head $ ps
  geoType _ = 5

data MultiPolygon = MultiPolygon (V.Vector (Geometry Polygon)) deriving (Show)

instance EWKBGeometry MultiPolygon where
  hasM (MultiPolygon ps) = hasM . V.head $ ps
  hasZ (MultiPolygon ps) = hasZ . V.head $ ps
  geoType _ = 6

data Geometry a = Geometry {
    srid :: SRID
  , geometry :: a
} deriving (Show)

instance EWKBGeometry a => EWKBGeometry (Geometry a) where
  hasM (Geometry s g) = hasM g 
  hasZ (Geometry s g) = hasZ g 
  geoType _ = 6



{-data Geometry a where-}
  {-GeoPoint :: SRID -> a -> Geometry a-}
  {-GeoLineString :: SRID -> LineString -> Geometry LineString -}
  {-GeoPolygon :: SRID -> Polygon -> Geometry Polygon-}
  {-GeoMultiPoint :: SRID -> MultiPoint -> Geometry MultiPoint-}
  {-GeoMultiLineString :: SRID -> MultiLineString -> Geometry MultiLineString-}
  {-GeoMultiPolygon :: SRID -> MultiPolygon -> Geometry MultiPolygon -}


{-instance Show a => Show (Geometry a) where-}
  {-show (GeoPoint s p) = show p-}
  {-show (GeoLineString s l) = show l-}
  {-show (GeoPolygon s p) = show p-}
  {-show (GeoMultiPoint s p) = show p-}
  {-show (GeoMultiLineString s l) = show l-}
  {-show (GeoMultiPolygon s p) = show p-}


