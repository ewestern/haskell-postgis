{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}

module Postgis.Types where

import qualified Data.Vector as V
import Data.Aeson
import Development.Placeholders
import qualified Data.Text as T

data Endian = BigEndian | LittleEndian deriving (Show)

data Point = WKBPoint {
	_x :: Double,
	_y :: Double,
	_m :: Maybe Double,
	_z :: Maybe Double
} deriving (Show)

instance ToJSON Point where
  toJSON (WKBPoint x y m z) = toJSON [x, y]

instance FromJSON Point where
  parseJSON (Object v) = $notImplemented

data LinearRing = LinearRing Int (V.Vector Point) deriving (Show)

instance ToJSON LinearRing where
  toJSON (LinearRing i vs) = $notImplemented

instance FromJSON LinearRing where
  parseJSON (Object v) = $notImplemented

data PointGeometry = PointGeometry Header Point deriving (Show)

instance ToJSON PointGeometry where
  toJSON (PointGeometry h p) = object ["type" .= ("Point" :: T.Text), "coordinates" .= (toJSON p)]


instance FromJSON PointGeometry where
  parseJSON (Object v) = $notImplemented

data LineStringGeometry = LineStringGeometry {
	_lineStringHeader :: Header,
	_numPoints :: Int,
	_points :: V.Vector Point
} deriving (Show)


instance FromJSON LineStringGeometry where
  parseJSON (Object v) = $notImplemented

instance ToJSON LineStringGeometry where
  toJSON (LineStringGeometry head np points) = object ["type" .= ("LineString" :: T.Text), "coordinates" .=  V.map toJSON points]

data PolygonGeometry = PolygonGeometry {
	_polygonHeader :: Header,
	_numRings :: Int,
	_rings :: V.Vector LinearRing
} deriving (Show)

instance ToJSON PolygonGeometry where
  toJSON (PolygonGeometry ph nr r) = $notImplemented

instance FromJSON PolygonGeometry where
  parseJSON (Object v) = $notImplemented

data MultiPointGeometry = MultiPointGeometry {
	_numPointGeometries :: Int,
	_pointGeometries :: V.Vector PointGeometry
} deriving (Show)

instance ToJSON MultiPointGeometry where
  toJSON (MultiPointGeometry np pg) = $notImplemented


instance FromJSON MultiPointGeometry where
  parseJSON (Object v) = $notImplemented

data MultiLineStringGeometry = MultiLineStringGeometry {
	_numLineStrings :: Int,
	_lineStrings :: V.Vector LineStringGeometry
} deriving (Show)

instance ToJSON MultiLineStringGeometry where
  toJSON (MultiLineStringGeometry nl ls) = $notImplemented


instance FromJSON MultiLineStringGeometry where
  parseJSON (Object v) = $notImplemented

data MultiPolygonGeometry = MultiPolygonGeometry {
	_numPolygons :: Int,
	_polygons :: V.Vector PolygonGeometry
} deriving (Show)

instance ToJSON MultiPolygonGeometry where
  toJSON (MultiPolygonGeometry np ps) = $notImplemented


instance FromJSON MultiPolygonGeometry where
  parseJSON (Object v) = $notImplemented

data Geometry = Point PointGeometry | LineString LineStringGeometry | Polygon PolygonGeometry | MultiPoint MultiPointGeometry |  MultiLineString MultiLineStringGeometry | MultiPolygon MultiPolygonGeometry deriving (Show)

instance ToJSON Geometry where
  toJSON (Point x) = toJSON x 
  toJSON (LineString x) = toJSON x 
  toJSON (Polygon x) = toJSON x 
  toJSON (MultiPoint x) = toJSON x 
  toJSON (MultiLineString x) = toJSON x 
  toJSON (MultiPolygon x) = toJSON x 


instance FromJSON Geometry where
  parseJSON = $notImplemented

data Header = Header {
	_byteOrder :: Endian,
	_geoType :: Int,
	_srid :: Maybe Int
} deriving (Show)

