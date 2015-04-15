{-# LANGUAGE OverloadedStrings #-}
import Database.Postgis
import Control.Applicative
import Data.Serialize
import Database.PostgreSQL.Simple.ToRow
import Database.PostgreSQL.Simple.ToField
import Database.PostgreSQL.Simple.FromRow
import Database.PostgreSQL.Simple.FromField hiding (Binary)
import Database.PostgreSQL.Simple
import Blaze.ByteString.Builder.ByteString
import qualified Data.ByteString as BS
import qualified Data.Vector as V
import Database.Postgis.Utils
import Database.Postgis.Serialize
import System.Endian
import Data.Word
import Data.Binary.IEEE754

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
sr = "E6100000"  :: BS.ByteString
x = "BCF36F97FDDA53C0" :: BS.ByteString
y = "42E207420D254540" :: BS.ByteString

badX = "c0537e19186a5a1b" :: BS.ByteString
badY = "404549f6b6ab2254" :: BS.ByteString
testGeo = GeoPoint (Just 4326) (Point {_x = -77.97028170000014, _y = 42.57784159999997, _z = Nothing, _m = Nothing})

tTable = TestTable "test" testGeo
main :: IO ()
main = do
   conn <- connectPostgreSQL "dbname=nyc host=localhost user=petefrance password=ellivretrop1"
   {-conn <- connectPostgreSQL "dbname=trailio host=localhost user=trailio password=trailio"-}

   {-(y:ys) <- query_ conn "select (geometry) from osm_places"-}
   let (GeoPoint s g) = testGeo 
   print $ show $ _x g
   print $ runPut $ writeNum $ _x g
   {-putStrLn $ show (y :: TrailTable) -}
   print $ writeGeometry $ testGeo 
   execute conn "insert into public.test (geometry) values (?)" (Only $ testGeo)
   {-(x:xs) <- query_ conn "select name, geometry from test limit 1"-}
   {-putStrLn $ show (x :: TestTable)-}
   {-print $ (geo x) == testGeo-}
  
   {-xs <- query_ conn "select geometry from osm_admin limit 2"-}
   {-putStrLn $ take 200 $ show  (head xs :: TrailTable)-}
   {-print $ (head xs :: TrailTable) -}
   return ()
