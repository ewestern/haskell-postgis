{-# LANGUAGE TemplateHaskell, OverloadedStrings #-}

module Database.Postgis.WKBTypes where


import qualified Database.Postgis.Geometry as G
import qualified Data.Vector as V
import Development.Placeholders
import System.Endian (Endianness)
import qualified Data.Text as T

data LinearRing = LinearRing Int (V.Vector G.Point) deriving (Show)

data Point = Point Header G.Point deriving (Show)

data LineString = LineString {
	_lineStringHeader :: Header,
	_numPoints :: Int,
	_points :: V.Vector G.Point
} deriving (Show)

data Polygon = Polygon {
	_polygonHeader :: Header,
	_numRings :: Int,
	_rings :: V.Vector LinearRing
} deriving (Show)

data MultiPoint = MultiPoint {
  _multiPointHeader :: Header,
	_numPointGeometries :: Int,
	_pointGeometries :: V.Vector Geometry
} deriving (Show)


data MultiLineString = MultiLineString {
  _multiLineStringHeader :: Header,
	_numLineStrings :: Int,
	_lineStrings :: V.Vector Geometry
} deriving (Show)


data MultiPolygon = MultiPolygon {
  _multiPolygonHeader :: Header,
	_numPolygons :: Int,
	_polygons :: V.Vector Geometry
} deriving (Show)

data Geometry = PointGeometry Point | LineStringGeometry LineString | PolygonGeometry Polygon | MultiPointGeometry MultiPoint | MultiLineStringGeometry  MultiLineString | MultiPolygonGeometry MultiPolygon deriving (Show)

data Header = Header {
	_byteOrder :: Endianness,
	_geoType :: Int,
	_srid :: Maybe Int
} deriving (Show)

