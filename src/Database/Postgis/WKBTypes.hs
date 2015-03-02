{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}

module Database.Postgis.WKBTypes where

import qualified Data.Vector as V
import Development.Placeholders
import System.Endian (Endianness)
import qualified Data.Text as T

{-data Endian = BigEndian | LittleEndian deriving (Show)-}

data WKBPoint = WKBPoint {
	_x :: {-# UNPACK #-} !Double,
	_y :: {-# UNPACK #-} !Double,
	_m :: Maybe Double,
	_z :: Maybe Double
} deriving (Show)

data LinearRing = LinearRing Int (V.Vector WKBPoint) deriving (Show)

type LineSegment = (Int, V.Vector WKBPoint)

data PointGeometry = PointGeometry Header WKBPoint deriving (Show)

data LineStringGeometry = LineStringGeometry {
	_lineStringHeader :: Header,
	_numPoints :: Int,
	_points :: V.Vector WKBPoint
} deriving (Show)

data PolygonGeometry = PolygonGeometry {
	_polygonHeader :: Header,
	_numRings :: Int,
	_rings :: V.Vector LinearRing
} deriving (Show)

data MultiPointGeometry = MultiPointGeometry {
	_numPointGeometries :: Int,
	_pointGeometries :: V.Vector Geometry
} deriving (Show)


data MultiLineStringGeometry = MultiLineStringGeometry {
	_numLineStrings :: Int,
	_lineStrings :: V.Vector Geometry
} deriving (Show)


data MultiPolygonGeometry = MultiPolygonGeometry {
	_numPolygons :: Int,
	_polygons :: V.Vector Geometry
} deriving (Show)

data Geometry = Point PointGeometry | LineString LineStringGeometry | Polygon PolygonGeometry | MultiPoint MultiPointGeometry |  MultiLineString MultiLineStringGeometry | MultiPolygon MultiPolygonGeometry deriving (Show)

data Header = Header {
	_byteOrder :: Endianness,
	_geoType :: Int,
	_srid :: Maybe Int
} deriving (Show)

