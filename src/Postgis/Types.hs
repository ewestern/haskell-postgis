module Postgis.Types where
import qualified Data.Vector as V

data Endian = BigEndian | LittleEndian deriving (Show)

data Point = WKBPoint {
	_x :: Double,
	_y :: Double,
	_m :: Maybe Double,
	_z :: Maybe Double
} deriving (Show)



data LinearRing = LinearRing Int (V.Vector Point) deriving (Show)

data PointGeometry = PointGeometry Header Point deriving (Show)

data LineStringGeometry = LineStringGeometry {
	_lineStringHeader :: Header,
	_numPoints :: Int,
	_points :: V.Vector Point
} deriving (Show)

data PolygonGeometry = PolygonGeometry {
	_polygonHeader :: Header,
	_numRings :: Int,
	_rings :: V.Vector LinearRing
} deriving (Show)

data MultiPointGeometry = MultiPointGeometry {
	_numPointGeometries :: Int,
	_pointGeometries :: V.Vector PointGeometry
} deriving (Show)

data MultiLineStringGeometry = MultiLineStringGeometry {
	_numLineStrings :: Int,
	_lineStrings :: V.Vector LineStringGeometry
} deriving (Show)

data MultiPolygonGeometry = MultiPolygonGeometry {
	_numPolygons :: Int,
	_polygons :: V.Vector PolygonGeometry
} deriving (Show)

data Geometry = Point PointGeometry | LineString LineStringGeometry | Polygon PolygonGeometry | MuliPoint MultiPointGeometry |  MultiLineString MultiLineStringGeometry | MultiPolygon MultiPolygonGeometry deriving (Show)

data Header = Header {
	_byteOrder :: Endian,
	_geoType :: Int,
	_srid :: Maybe Int
} deriving (Show)


