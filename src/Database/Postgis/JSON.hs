{-# LANGUAGE TemplateHaskell #-}

module Database.Postgis.Simple.JSON (
  ToJSON (..),
  FromJSON (..)
)

import Data.Aeson
import Database.Postgis.Simple.Types
import Development.Placeholders

instance ToJSON WKBPoint where
  toJSON (WKBPoint x y m z) = toJSON [x, y]

instance FromJSON WKBPoint where
  parseJSON (Object v) = $notImplemented

instance ToJSON LinearRing where
  toJSON (LinearRing i vs) = toJSON $ V.map toJSON vs 

instance FromJSON LinearRing where
  parseJSON (Object v) = $notImplemented

instance ToJSON PointGeometry where
  toJSON (PointGeometry h p) = object ["type" .= ("Point" :: T.Text), "coordinates" .= (toJSON p)]


instance FromJSON PointGeometry where
  parseJSON (Object v) = $notImplemented

instance FromJSON LineStringGeometry where
  parseJSON (Object v) = $notImplemented

instance ToJSON LineStringGeometry where
  toJSON (LineStringGeometry head np points) = object ["type" .= ("LineString" :: T.Text), "coordinates" .=  V.map toJSON points]

instance ToJSON PolygonGeometry where
  toJSON (PolygonGeometry head num rings) = object ["type" .= ("Polygon" :: T.Text), "coordinates" .= V.map toJSON rings]

instance FromJSON PolygonGeometry where
  parseJSON (Object v) = $notImplemented

instance ToJSON MultiPointGeometry where
  toJSON (MultiPointGeometry np pg) = object ["type" .= ("MultiPoint" :: T.Text), "coordinates" .= V.map toJSON pg]

instance FromJSON MultiPointGeometry where
  parseJSON (Object v) = $notImplemented

instance ToJSON MultiLineStringGeometry where
  toJSON (MultiLineStringGeometry nl ls) = object ["type" .= ("MultiLineString" :: T.Text), "coordinates" .= V.map toJSON ls]

instance FromJSON MultiLineStringGeometry where
  parseJSON (Object v) = $notImplemented

instance ToJSON MultiPolygonGeometry where
  toJSON (MultiPolygonGeometry np ps) = object ["type" .= ("MultiPolygon" :: T.Text), "coordinates" .= V.map toJSON ps]

instance FromJSON MultiPolygonGeometry where
  parseJSON (Object v) = $notImplemented

instance ToJSON Geometry where
  toJSON (Point x) = toJSON x 
  toJSON (LineString x) = toJSON x 
  toJSON (Polygon x) = toJSON x 
  toJSON (MultiPoint x) = toJSON x 
  toJSON (MultiLineString x) = toJSON x 
  toJSON (MultiPolygon x) = toJSON x 

instance FromJSON Geometry where
  parseJSON = $notImplemented

