{-# LANGUAGE OverloadedStrings #-}
import Database.Postgis
import Control.Applicative
import Database.PostgreSQL.Simple.FromRow
import Database.PostgreSQL.Simple.FromField
import Database.PostgreSQL.Simple


data TrailTable = TrailTable {
	geometry :: Geometry
} deriving (Show)

instance FromRow TrailTable where
	fromRow = TrailTable <$> field 

instance FromField Geometry where
	fromField f m = case m of
              Just bs -> return $ readGeometry bs
              Nothing -> error "can't read field" 

main :: IO ()
main = do
   conn <- connectPostgreSQL "dbname=trailio host=localhost user=trailio password=trailio"
   xs <- query_ conn "select geometry from osm_trails limit 2"
   print $ (head xs :: TrailTable) 
   return ()
