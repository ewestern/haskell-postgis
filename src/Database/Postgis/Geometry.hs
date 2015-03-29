{-# LANGUAGE ExistentialQuantification, FlexibleInstances, GADTs #-}

module Database.Postgis.Geometry where

import qualified Data.Vector as V
import Development.Placeholders
import qualified Data.Text as T


{-Linear rings—Rings are simple and closed, which means that linear rings may not self intersect.-}

{-Polygons—No two linear rings in the boundary of a polygon may cross each other. The linear rings in the boundary of a polygon may intersect, at most, at a single point but only as a tangent.--}

{-Multipolygons—The interiors of two polygons that are elements of a multipolygon may not intersect. The boundaries of any two polygons that are elements of a multipolygon may touch at only a finite number of points.-}

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

data MultiPoint = MultiPoint (V.Vector Point)

instance Show MultiPoint where
  show (MultiPoint ps) = show ps

instance EWKBGeometry MultiPoint where
  hasM (MultiPoint ps) = hasM . V.head $ ps
  hasZ (MultiPoint ps) = hasZ . V.head $ ps
  geoType _ = 4

data MultiLineString = MultiLineString (V.Vector LineString) deriving (Show)

instance EWKBGeometry MultiLineString where
  hasM (MultiLineString ps) = hasM . V.head $ ps
  hasZ (MultiLineString ps) = hasZ . V.head $ ps
  geoType _ = 5

data MultiPolygon = MultiPolygon (V.Vector Polygon) deriving (Show)

instance EWKBGeometry MultiPolygon where
  hasM (MultiPolygon ps) = hasM . V.head $ ps
  hasZ (MultiPolygon ps) = hasZ . V.head $ ps
  geoType _ = 6

instance Show Geometry where
  show (GeoPoint s g) = show g 
  show (GeoLineString s g) = show g 
  show (GeoPolygon s g) = show g 
  show (GeoMultiPoint s g) = show g 
  show (GeoMultiLineString s g) = show g 
  show (GeoMultiPolygon s g) = show g 

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
  | GeoMultiPolygon SRID MultiPolygon

{-data Geometry a where-}
  {-GeoPoint :: SRID -> Point -> Geometry Point-}
  {-GeoLineString :: SRID -> LineString -> Geometry LineString-}
  {-GeoPolygon :: SRID -> Polygon -> Geometry Polygon-}
  {-GeoMultiLineString :: SRID -> MultiLineString -> Geometry MultiLineString-}
  {-GeoMultiPoint :: SRID -> MultiPoint -> Geometry MultiPoint-}
  {-GeoMultiPolygon :: SRID -> MultiPolygon -> Geometry MultiPolygon-}


