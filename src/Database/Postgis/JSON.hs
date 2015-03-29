
module Database.Postgis.Simple.JSON (
  ToJSON (..),
  FromJSON (..)
)

import Data.Aeson
import Database.Postgis.Geometry
import Development.Placeholders


instance ToJSON Point where
  toJSON (Point x y m z) = toJSON [x, y, m, z]

{-object ["type" .= ("Point" :: T.Text), "coordinates" .= (toJSON [x, y, m, z])]-}

instance FromJSON Point where
  parseJSON (Object v) = $notImplemented

instance ToJSON LinearRing where
  toJSON (LinearRing vs) = toJSON $ V.map toJSON vs 

instance FromJSON LinearRing where
  parseJSON (Object v) = $notImplemented

instance FromJSON LineString where
  parseJSON (Object v) = $notImplemented

instance ToJSON LineString where
  toJSON (LineString points) = object ["type" .= ("LineString" :: T.Text), "coordinates" .=  V.map toJSON points]

instance ToJSON Polygon where
  toJSON (Polygon rings) = object ["type" .= ("Polygon" :: T.Text), "coordinates" .= V.map toJSON rings]

instance FromJSON Polygon where
  parseJSON (Object v) = $notImplemented

---
instance ToJSON MultiPoint where
  toJSON (MultiPoint pg) = object ["type" .= ("MultiPoint" :: T.Text), "coordinates" .= V.map toJSON pg]

instance FromJSON MultiPoint where
  parseJSON (Object v) = $notImplemented

instance ToJSON MultiLineString where
  toJSON (MultiLineString ls) = object ["type" .= ("MultiLineString" :: T.Text), "coordinates" .= V.map toJSON ls]

instance FromJSON MultiLineStringGeometry where
  parseJSON (Object v) = $notImplemented

instance ToJSON MultiPolygon where
  toJSON (MultiPolygon np ps) = object ["type" .= ("MultiPolygon" :: T.Text), "coordinates" .= V.map toJSON ps]

instance FromJSON MultiPolygon where
  parseJSON (Object v) = $notImplemented

instance ToJSON Geometry where
  toJSON (GeoPoint x) = toJSON x
  toJSON (GeoLineString x) = toJSON x
  toJSON (GeoPolygon x) = toJSON x
  toJSON (GeoMultiPoint x) = toJSON x
  toJSON (GeoMultiLineString x) = toJSON x
  toJSON (GeoMultiPolygon x) = toJSON x

instance FromJSON Geometry where
  parseJSON = $notImplemented

