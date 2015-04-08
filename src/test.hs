{-# LANGUAGE OverloadedStrings #-}
import Database.Postgis
import Control.Applicative
import Database.PostgreSQL.Simple.ToRow
import Database.PostgreSQL.Simple.ToField
import Database.PostgreSQL.Simple.FromRow
import Database.PostgreSQL.Simple.FromField hiding (Binary)
import Database.PostgreSQL.Simple
import Blaze.ByteString.Builder.ByteString

import qualified Data.Vector as V

data TrailTable = TrailTable {
	geometry :: Geometry
} deriving (Show)

data TestTable = TestTable {
    name :: String
  , geo :: Geometry
} deriving (Show)

instance FromRow TestTable where
	fromRow = TestTable <$> field  <*> field

instance FromRow TrailTable where
	fromRow = TrailTable <$> field 

instance ToRow TestTable where
  toRow (TestTable n g)  = [toField n, toField g]

instance ToField Geometry where
  {-toField = toField . Binary . writeGeometry -}
  toField  =  Plain . fromByteString . writeGeometry 

instance FromField Geometry where
	fromField f m = case m of
              Just bs -> return $ readGeometry bs
              Nothing -> error "can't read field" 

b = "0100000003000010e6"
testGeo = GeoPolygon (Just 4326) (Polygon (V.fromList [V.fromList [Point {_x = -73.910675070397, _y = 42.12729292567653, _z = Just 1.0, _m = Nothing},Point {_x = -73.91057004515024, _y = 42.127415972015115, _z = Just 1.0, _m = Nothing}]]))

tTable = TestTable "test" testGeo
main :: IO ()
main = do
   conn <- connectPostgreSQL "dbname=nyc host=localhost user=petefrance password=ellivretrop1"

   {-conn <- connectPostgreSQL "dbname=trailio host=localhost user=trailio password=trailio"-}
   (y:ys) <- query_ conn "select (geometry) from osm_places"
   putStrLn $ show (y :: TrailTable) 
   print $ writeGeometry testGeo
   execute conn "insert into public.test (geometry) values (?)" (Only testGeo)
   {-(x:xs) <- query_ conn "select name, geometry from test limit 1"-}
   {-putStrLn $ show (x :: TestTable)-}
   {-print $ (geo x) == testGeo-}
  
   {-xs <- query_ conn "select geometry from osm_admin limit 2"-}
   {-putStrLn $ take 200 $ show  (head xs :: TrailTable)-}
   {-print $ (head xs :: TrailTable) -}
   return ()
