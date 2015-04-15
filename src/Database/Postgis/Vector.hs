{-# LANGUAGE TypeFamilies #-}
module Database.Postgis.Vector where

import Database.Postgis.Geometry
import Data.VectorSpace
import Data.AdditiveGroup
import Data.AffineSpace
import qualified Data.Vector as V

instance Ord Point where
-- for use in convex hull algorithm
  compare (Point2 x y) (Point2 x2 y2)
    | x < x2 = LT
    | x > x2 = GT
    | y < y2 = LT
    | otherwise = EQ 


vConcatMap :: V.Vector (V.Vector a) -> V.Vector a
vConcatMap = (=<<) id

dump :: Geometry -> V.Vector Point
dump g = case g of 
  GeoPoint s p ->  V.singleton p
  GeoLineString s (LineString ps) -> ps
  GeoPolygon s (Polygon ls) -> vConcatMap ls 
  GeoMultiPoint s (MultiPoint ps) -> ps
  GeoMultiLineString s (MultiLineString ls) -> V.map (\(LineString v) -> v) ls >>= id  
  GeoMultiPolygon s (MultiPolygon ps) -> V.map (\(Polygon ls) -> vConcatMap ls) ps >>= id 

instance AdditiveGroup Point where
  zeroV = Point2 zeroV zeroV
  (Point2 x1 y1) ^+^ (Point2 x2 y2) = Point2 (x1 + x2) (y1 + y2)
  negateV (Point2 x y) = Point2 (-x) (-y) 

instance AffineSpace Point where
  type Diff Point = (Double, Double)
  (Point2 x1 y1) .-. (Point2 x2 y2) = (x1 .-. x2, y1 .-. y2)
  (Point2 x1 y1) .+^ (x2, y2) = Point2 (x1 .-. x2) (y1 .-. y2)

instance VectorSpace Point where
  type Scalar Point = Double
  d *^ (Point2 x y) = Point2 (x * d) (y * d)

instance InnerSpace Point where
  (Point2 x y) <.> (Point2 x2 y2) = (x * x2) + (y * y2)


{-cross :: AffineSpace a, VectorSpace a-}
{-instance VectorSpace Point where-}
  {-type Scalar Point =  -}
