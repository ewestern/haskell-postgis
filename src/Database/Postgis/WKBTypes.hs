{-# LANGUAGE TemplateHaskell, OverloadedStrings #-}

module Database.Postgis.WKBTypes where


import qualified Database.Postgis.Geometry as G
import qualified Data.Vector as V
import Development.Placeholders
import System.Endian (Endianness)
import qualified Data.Text as T

data LinearRing = LinearRing Int (V.Vector G.Point) deriving (Show)

data Point = Point {
    pointHeader :: Header 
  , point :: G.Point 
} deriving (Show)

data LineString = LineString {
	lineStringHeader :: Header,
	numPoints :: Int,
	points :: V.Vector G.Point
} deriving (Show)

data Polygon = Polygon {
	polygonHeader :: Header,
	numRings :: Int,
	rings :: V.Vector LinearRing
} deriving (Show)

data MultiPoint = MultiPoint {
  multiPointHeader :: Header,
	numPointGeometries :: Int,
	pointGeometries :: V.Vector Point 
} deriving (Show)


data MultiLineString = MultiLineString {
  multiLineStringHeader :: Header,
	numLineStrings :: Int,
	lineStrings :: V.Vector LineString 
} deriving (Show)


data MultiPolygon = MultiPolygon {
    multiPolygonHeader :: Header
	, numPolygons :: Int
	, polygons :: V.Vector Polygon 
} deriving (Show)

data Geometry = PointGeometry Point | LineStringGeometry LineString | PolygonGeometry Polygon | MultiPointGeometry MultiPoint | MultiLineStringGeometry  MultiLineString | MultiPolygonGeometry MultiPolygon deriving (Show)

data Header = Header {
	  byteOrder :: Endianness
	, geoType :: Int
	, srid :: (Maybe Int)
} deriving (Show)

