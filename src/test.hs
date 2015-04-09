{-# LANGUAGE OverloadedStrings #-}
import Database.Postgis
import Control.Applicative
import Database.PostgreSQL.Simple.ToRow
import Database.PostgreSQL.Simple.ToField
import Database.PostgreSQL.Simple.FromRow
import Database.PostgreSQL.Simple.FromField hiding (Binary)
import Database.PostgreSQL.Simple
import Blaze.ByteString.Builder.ByteString
import qualified Data.ByteString as BS
import qualified Data.Vector as V
import Database.Postgis.Utils
import Data.Word

data TrailTable = TrailTable {
	{-geometry :: BS.ByteString -}
  geometry :: Geometry
} deriving (Show)

data TestTable = TestTable {
    name :: String
  {-, geo :: BS.ByteString-}
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

p ="0101000020E6100000BCF36F97FDDA53C042E207420D254540" 
end = "01"
typ = "01000020" :: BS.ByteString
{-testGeo = GeoPolygon (Just 4326) (Polygon (V.fromList [V.fromList [Point {_x = -73.910675070397, _y = 42.12729292567653, _z = Just 1.0, _m = Nothing},Point {_x = -73.91057004515024, _y = 42.127415972015115, _z = Just 1.0, _m = Nothing}]]))-}


testGeo = GeoPoint (Just 4326) (Point {_x = -77.97028170000014, _y = 42.57784159999997, _z = Nothing, _m = Nothing})

tTable = TestTable "test" testGeo
main :: IO ()
main = do
   conn <- connectPostgreSQL "dbname=nyc host=localhost user=petefrance password=ellivretrop1"

   {-conn <- connectPostgreSQL "dbname=trailio host=localhost user=trailio password=trailio"-}
   (y:ys) <- query_ conn "select (geometry) from osm_places"
   putStrLn $ show (y :: TrailTable) 
   print $ writeGeometry $ geometry y 
   execute conn "insert into public.test (geometry) values (?)" (Only $ geometry y)
   {-(x:xs) <- query_ conn "select name, geometry from test limit 1"-}
   {-putStrLn $ show (x :: TestTable)-}
   {-print $ (geo x) == testGeo-}
  
   {-xs <- query_ conn "select geometry from osm_admin limit 2"-}
   {-putStrLn $ take 200 $ show  (head xs :: TrailTable)-}
   {-print $ (head xs :: TrailTable) -}
   return ()
