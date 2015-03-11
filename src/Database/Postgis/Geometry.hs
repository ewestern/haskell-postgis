module Database.Postgis.Geometry where

import qualified Data.Vector as V
import Development.Placeholders
import qualified Data.Text as T

type SRID = Int

data Point = Point {
	_x :: {-# UNPACK #-} !Double,
	_y :: {-# UNPACK #-} !Double,
  _m :: Maybe Double, 
  _z :: Maybe Double 
} deriving (Show)

data Header = Header {
    byteOrder :: Endianness
  , geoType :: Int
  , srid :: (Maybe Int)
} deriving (Show)



data LineString = LineString (V.Vector Point) deriving (Show)


-- todo, would like to dependently type this
data LinearRing = LinearRing (V.Vector Point) deriving (Show)


data Polygon = Polygon (V.Vector LinearRing) deriving (Show)

data MultiPoint = MultiPoint (V.Vector Point) deriving (Show)

data MultiLineString = MultiLineString (V.Vector LineString) deriving (Show)

data MultiPolygon = MultiPolygon (V.Vector Polygon) deriving (Show)

newtype Geometry a = Geometry {
  geometry :: a
} deriving (Show)
{-data Geometry = PointGeometry Point | LineStringGeometry LineString | PolygonGeometry Polygon | MultiPointGeometry MultiPoint | MultiLineStringGeometry  MultiLineString | MultiPolygonGeometry MultiPolygon deriving (Show)-}

data Feature = Feature (Maybe SRID) Geometry

