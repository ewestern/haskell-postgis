{-# LANGUAGE GADTs, TypeFamilies #-}
{-# LANGUAGE DeriveDataTypeable #-}

module Database.Postgis.Geometry where

import qualified Data.Vector as V
import Data.Word
import Data.Data
import Data.Typeable

{-Linear rings—Rings are simple and closed, which means that linear rings may not self intersect.-}

{-Polygons—No two linear rings in the boundary of a polygon may cross each other. The linear rings in the boundary of a polygon may intersect, at most, at a single point but only as a tangent.--}

{-Multipolygons—The interiors of two polygons that are elements of a multipolygon may not intersect. The boundaries of any two polygons that are elements of a multipolygon may touch at only a finite number of points.-}

type SRID = Maybe Int 


class EWKBGeometry a where
  hasM :: a -> Bool
  hasZ :: a -> Bool
  geoType :: a -> Word32 


data Point = Point  {
    _x :: Double
  , _y :: Double
  , _z :: Maybe Double
  , _m :: Maybe Double
} deriving (Data, Typeable, Show, Eq)

instance EWKBGeometry Point where
  hasM (Point x y z m) = m /= Nothing 
  hasZ (Point x y z m) = z /= Nothing 
  geoType _ = 1

type LinearRing = V.Vector Point

isClosed :: V.Vector Point -> Bool
isClosed v = V.head v == V.last v


data LineString = LineString (V.Vector Point) deriving (Data, Typeable, Show, Eq)


instance EWKBGeometry LineString where
  hasM (LineString ps) = hasM . V.head $ ps
  hasZ (LineString ps) = hasZ . V.head $ ps
  geoType _ = 2

data Polygon = Polygon (V.Vector LinearRing) deriving (Data, Typeable, Show, Eq)

hasMLinearRing :: LinearRing -> Bool
hasMLinearRing = hasM . V.head 

hasZLinearRing :: LinearRing -> Bool
hasZLinearRing = hasZ . V.head 

instance EWKBGeometry Polygon where
  hasM (Polygon ps) = hasMLinearRing . V.head $ ps
  hasZ (Polygon ps) = hasZLinearRing . V.head $ ps
  geoType _ = 3

data MultiPoint = MultiPoint (V.Vector Point) deriving (Data, Typeable, Show, Eq)

instance EWKBGeometry MultiPoint where
  hasM (MultiPoint ps) = hasM . V.head $ ps
  hasZ (MultiPoint ps) = hasZ . V.head $ ps
  geoType _ = 4

data MultiLineString = MultiLineString (V.Vector LineString) deriving (Data, Typeable, Show, Eq)

instance EWKBGeometry MultiLineString where
  hasM (MultiLineString ps) = hasM . V.head $ ps
  hasZ (MultiLineString ps) = hasZ . V.head $ ps
  geoType _ = 5

data MultiPolygon = MultiPolygon (V.Vector Polygon) deriving (Data, Typeable, Show, Eq)

instance EWKBGeometry MultiPolygon where
  hasM (MultiPolygon ps) = hasM . V.head $ ps
  hasZ (MultiPolygon ps) = hasZ . V.head $ ps
  geoType _ = 6

srid :: Geometry -> SRID
srid g = case g of
  GeoPoint s p -> s
  GeoLineString s l -> s
  GeoPolygon s p -> s
  GeoMultiPoint s p -> s
  GeoMultiLineString s m -> s
  GeoMultiPolygon s m -> s

data Geometry =
    GeoPoint SRID Point
  | GeoLineString SRID LineString
  | GeoPolygon SRID Polygon
  | GeoMultiLineString SRID MultiLineString
  | GeoMultiPoint SRID MultiPoint
  | GeoMultiPolygon SRID MultiPolygon deriving (Data, Typeable, Show, Eq)
