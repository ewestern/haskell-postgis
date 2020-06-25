{-# LANGUAGE GADTs, TypeFamilies #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE GeneralisedNewtypeDeriving #-}

module Database.Postgis.Geometry where

import qualified Data.Vector as V
import Data.Word
import Data.Data
import Data.Typeable
import Data.Maybe

{-Linear rings—Rings are simple and closed, which means that linear rings may not self intersect.-}

{-Polygons—No two linear rings in the boundary of a polygon may cross each other. The linear rings in the boundary of a polygon may intersect, at most, at a single point but only as a tangent.--}

{-Multipolygons—The interiors of two polygons that are elements of a multipolygon may not intersect. The boundaries of any two polygons that are elements of a multipolygon may touch at only a finite number of points.-}

type SRID = Maybe Int 


class EWKBGeometry a where
  hasM :: a -> Bool
  hasZ :: a -> Bool
  geoType :: a -> Word32 

data Position = Position {
    _x :: Double
  , _y :: Double
  , _z :: Maybe Double
  , _m :: Maybe Double
} deriving (Data, Typeable, Show, Eq)

newtype Point = Point Position deriving (Data, Typeable, Show, Eq, EWKBGeometry)

instance EWKBGeometry Position where
  hasM (Position x y z m) = isJust m
  hasZ (Position x y z m) = isJust z
  geoType _ = 1

type LinearRing = V.Vector Position

isClosed :: V.Vector Position -> Bool
isClosed v = V.head v == V.last v


newtype LineString = LineString (V.Vector Position) deriving (Data, Typeable, Show, Eq)


instance EWKBGeometry LineString where
  hasM (LineString ps) = hasM . V.head $ ps
  hasZ (LineString ps) = hasZ . V.head $ ps
  geoType _ = 2

newtype Polygon = Polygon (V.Vector LinearRing) deriving (Data, Typeable, Show, Eq)

hasMLinearRing :: LinearRing -> Bool
hasMLinearRing = hasM . V.head 

hasZLinearRing :: LinearRing -> Bool
hasZLinearRing = hasZ . V.head 

instance EWKBGeometry Polygon where
  hasM (Polygon ps) = hasMLinearRing . V.head $ ps
  hasZ (Polygon ps) = hasZLinearRing . V.head $ ps
  geoType _ = 3

newtype MultiPoint = MultiPoint (V.Vector Position) deriving (Data, Typeable, Show, Eq)

instance EWKBGeometry MultiPoint where
  hasM (MultiPoint ps) = hasM . V.head $ ps
  hasZ (MultiPoint ps) = hasZ . V.head $ ps
  geoType _ = 4

newtype MultiLineString = MultiLineString (V.Vector LineString) deriving (Data, Typeable, Show, Eq)

instance EWKBGeometry MultiLineString where
  hasM (MultiLineString ps) = hasM . V.head $ ps
  hasZ (MultiLineString ps) = hasZ . V.head $ ps
  geoType _ = 5

newtype MultiPolygon = MultiPolygon (V.Vector Polygon) deriving (Data, Typeable, Show, Eq)

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
