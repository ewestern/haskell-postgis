{-# LANGUAGE OverloadedStrings #-}
import Database.PostgreSQL.Simple
import Postgis.Types
import Postgis.DB()
import Control.Applicative
import Database.PostgreSQL.Simple.FromRow

data TrailTable = TrailTable {
	geometry :: Geometry
} deriving (Show)

instance FromRow TrailTable where
	fromRow = TrailTable <$> field 

main :: IO ()
main = do
   conn <- connectPostgreSQL "dbname=trailio host=localhost user=trailio password=trailio"
   xs <- query_ conn "select geometry from osm_old_admin limit 2"
   print $ (head xs :: TrailTable) 
   return ()
