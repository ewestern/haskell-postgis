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

  
class Point a where
  dimensions :: a -> Int
  _x :: a -> Double
  _y :: a -> Double
  _z :: a -> Maybe Double
  _m :: a -> Maybe Double

data Point2D = Point2D {-# UNPACK #-} !Double  
                       {-# UNPACK #-} !Double deriving (Show)

instance Point Point2D where
  dimensions _ = 2
  _x (Point2D x _) = x 
  _y (Point2D _ y) = y 
  _z _ = Nothing
  _m _ = Nothing

instance EWKBGeometry Point2D where
  hasM _ = False
  hasZ _ = False
  geoType _ = 1

data Point3D = Point3DZ {-# UNPACK #-} !Double  
                        {-# UNPACK #-} !Double
                        {-# UNPACK #-} !Double
             | Point3DM {-# UNPACK #-} !Double  
                        {-# UNPACK #-} !Double
                        {-# UNPACK #-} !Double deriving (Show)


instance EWKBGeometry Point3D where
  hasM _ = True
  hasZ _ = False
  geoType _ = 1

instance Point Point3D where
  dimensions _ = 3
  _x (Point3DZ x _ _) = x 
  _x (Point3DM x _ _) = x
  _y (Point3DZ _ y _) = y 
  _y (Point3DM _ y _) = y 
  _z (Point3DZ _ _ z) = Just z 
  _z (Point3DM _ _ _) = Nothing 
  _m (Point3DM _ _ m) = Just m 
  _m (Point3DZ _ _ _) = Nothing  

data Point4D = Point4D {-# UNPACK #-} !Double  
                       {-# UNPACK #-} !Double
                       {-# UNPACK #-} !Double
                       {-# UNPACK #-} !Double deriving (Show)


instance EWKBGeometry Point4D where
  hasM _ = True
  hasZ _ = True
  geoType _ = 1

instance Point Point4D where
  dimensions _ = 4
  _x (Point4D x _ _ _) = x 
  _y (Point4D _ y _ _) = y 
  _z (Point4D _ _ z _) = Just z 
  _m (Point4D _ _ _ m) = Just m 

-- todo, would like to dependently type this
data LinearRing = forall a. (EWKBGeometry a, Point a, Show a) => LinearRing (V.Vector a) 

instance Show LinearRing where
  show (LinearRing vs) = show vs


{-instance EWKBGeometry LinearRing where-}
  {-hasM (LinearRing ps) = hasM . V.head $ ps-}
  {-hasZ (LinearRing ps) = hasZ . V.head $ ps-}

data LineString = forall a. (EWKBGeometry a, Point a, Show a) => LineString (V.Vector a) 

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

data MultiPoint = forall a. (EWKBGeometry a, Point a, Show a) => MultiPoint (V.Vector (Geometry a)) 

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

data Geometry a where
  GeoPoint :: Point a => SRID -> a -> Geometry a
  GeoLineString :: SRID -> LineString -> Geometry LineString 
  GeoPolygon :: SRID -> Polygon -> Geometry Polygon
  GeoMultiPoint :: SRID -> MultiPoint -> Geometry MultiPoint
  GeoMultiLineString :: SRID -> MultiLineString -> Geometry MultiLineString
  GeoMultiPolygon :: SRID -> MultiPolygon -> Geometry MultiPolygon 

{-data Geometry a = forall a. (EWKBGeometry a, Point a, Show a) => Geometry -}
    {-srid :: Maybe Int-}
  {-, geometry :: a-}
{-} -}

{-instance EWKBGeometry (Geometry a) where-}
  {-hasM (Geometry g) = hasM g -}
  {-hasZ (Geometry g) = hasZ g -}
  {-geoType _ = 0-}



instance Show a => Show (Geometry a) where
  show (GeoPoint s p) = show p
  show (GeoLineString s l) = show l
  show (GeoPolygon s p) = show p
  show (GeoMultiPoint s p) = show p
  show (GeoMultiLineString s l) = show l
  show (GeoMultiPolygon s p) = show p


  {-hasM (Geometry s g) = hasM g-}
  {-hasZ (Geometry s g) = hasZ g-}
  {-geoType _ = 0-}
