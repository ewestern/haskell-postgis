{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Database.Postgis.JSON (
  ToJSON (..),
  FromJSON (..)
) where

import Control.Applicative ((<|>))
import Data.Aeson
import Data.Aeson.Types (Parser)
import Database.Postgis.Geometry
import qualified Data.Vector as V
import qualified Data.Text as T
import Development.Placeholders
import Data.Maybe
import Data.Monoid ((<>))
import Data.Vector ((!), (!?))
import qualified Data.HashMap.Lazy as HM
import Data.Text.Read (decimal)

instance ToJSON Position where
  toJSON (Position x y m z) = toJSON $ catMaybes [Just x, Just y, m, z]

instance FromJSON Position where
  parseJSON = withArray "Position" $ \v' -> do
    v <- mapM parseJSON v'
    return $ Position (v ! 0) (v ! 1) (v !? 2) (v !? 3)

instance ToJSON Point where
  toJSON (Point position) = object
    [ "type" .= ("Point" :: T.Text)
    , "coordinates" .= toJSON position
    ]

instance FromJSON Point where
  parseJSON = withObject "Point" $ \o -> do
    ("Point" :: T.Text) <- o .: "type"
    cs <- o .: "coordinates"
    pos <- parseJSON cs
    return $ Point pos

instance FromJSON LineString where
  parseJSON = withObject "LineString" $ \o -> do
    ("LineString" :: T.Text) <- o .: "type"
    cs <- o .: "coordinates"
    vs <- mapM parseJSON cs
    return $ LineString vs

instance ToJSON LineString where
  toJSON (LineString points) = object ["type" .= ("LineString" :: T.Text), "coordinates" .=  V.map toJSON points]


--
instance ToJSON Polygon where
  toJSON (Polygon rings) = object ["type" .= ("Polygon" :: T.Text), "coordinates" .= V.map toJSON rings]

instance FromJSON Polygon where
  parseJSON = withObject "Polygon" $ \o -> do
    ("Polygon" :: T.Text) <- o .: "type"
    ls <- o .: "coordinates"
    cs <- mapM parseJSON ls
    return $ Polygon cs

---
instance ToJSON MultiPoint where
  toJSON (MultiPoint pg) = object ["type" .= ("MultiPoint" :: T.Text), "coordinates" .= V.map toJSON pg]

instance FromJSON MultiPoint where
  parseJSON = withObject "MultiPoint" $ \o -> do
    ("MultiPoint" :: T.Text) <- o .: "type"
    ls <- o .: "coordinates"
    cs <- mapM parseJSON ls
    return $ MultiPoint cs

instance ToJSON MultiLineString where
  toJSON (MultiLineString ls) = object ["type" .= ("MultiLineString" :: T.Text), "coordinates" .= V.map toJSON ls]

instance FromJSON MultiLineString where
  parseJSON = withObject "MultiLineString" $ \o -> do
    ("MultiLineString" :: T.Text) <- o .: "type"
    ls <- o .: "coordinates"
    cs <- mapM parseJSON ls
    return $ MultiLineString cs

instance ToJSON MultiPolygon where
  toJSON (MultiPolygon ps) = object ["type" .= ("MultiPolygon" :: T.Text), "coordinates" .= V.map toJSON ps]

instance FromJSON MultiPolygon where
  parseJSON = withObject "MultiPolygon" $ \o -> do
    ("MultiPolygon" :: T.Text) <- o .: "type"
    ls <- o .: "coordinates"
    cs <- mapM parseJSON ls
    return $ MultiPolygon cs

addKeyToValue :: Value -> T.Text -> Value -> Maybe Value
addKeyToValue (Object hm) k v = Just . Object $ HM.insert k v hm
addKeyToValue _ _ _ = Nothing

go :: ToJSON a => SRID -> a -> Value
go (Just s) x =
    let v = toJSON x
    in fromMaybe v $ addKeyToValue v "crs" $ sridToJson s
go Nothing x = toJSON x

instance ToJSON Geometry where
  toJSON (GeoPoint s x) =  go s x
  toJSON (GeoLineString s x) =  go s x
  toJSON (GeoPolygon s x) =  go s x
  toJSON (GeoMultiPoint s x) =  go s x
  toJSON (GeoMultiLineString s x) =  go s x
  toJSON (GeoMultiPolygon s x) =  go s x

sridToJson srid =
  object ["type" .= ("name" :: T.Text), "properties" .= object ["name" .= ("ESPG:" <> show srid  :: String)] ]


parseCRS :: Value -> Parser (Maybe Int)
parseCRS = withObject "crs" $ \o ->  do
  crs <- o .: "crs"
  ("name"::T.Text) <- crs .: "type"
  prop <- crs .: "properties"
  espg <-  prop .: "name"
  let (x:y:xs) = T.split (':' ==) espg
  case decimal y of
    Left e ->  return Nothing
    Right (v,_) -> return $ Just v


instance FromJSON Geometry where
  parseJSON o
    =   GeoPoint <$> parseCRS o <*> parseJSON o
    <|> GeoLineString <$> parseCRS o <*> parseJSON o
    <|> GeoPolygon <$> parseCRS o <*> parseJSON o
    <|> GeoMultiPoint <$> parseCRS o <*> parseJSON o
    <|> GeoMultiLineString <$> parseCRS o <*> parseJSON o
    <|> GeoMultiPolygon <$> parseCRS o <*> parseJSON o
