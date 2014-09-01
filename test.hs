{-# LANGUAGE OverloadedStrings #-}
import Database.PostgreSQL.Simple
import Postgis.Parser
import Postgis.Types

import Control.Applicative
import Database.PostgreSQL.Simple.FromRow
import Database.PostgreSQL.Simple.FromField
import qualified Data.Text as T
import qualified Data.ByteString.Lazy as B
import qualified Data.ByteString.Lazy.Char8 as CL
import qualified Data.ByteString.Char8 as C
import qualified Data.ByteString as BS
{-import Data.Binary.Get-}
import Data.Serialize.Get
import Data.Word
import Control.Monad
import Text.Parsec hiding (State)
import Text.Parsec.String (Parser)
import Numeric
import Data.Bits
import qualified Data.Vector as V
import Data.Maybe
import Data.Binary (decode)

data TrailTable = TrailTable {
	{-id :: Integer,-}
	{-osm_id :: Integer,-}
	{-name :: T.Text,-}
	{-type_ :: T.Text,-}
	--geometry :: CL.ByteString,
	geometry :: Geometry
	{-geometry_z :: Maybe C.ByteString-}
} deriving (Show)

instance FromRow TrailTable where
	fromRow = TrailTable <$> field 



hello :: IO ()
hello = do
   conn <- connectPostgreSQL "dbname=trailio host=localhost user=trailio password=trailio"
   xs <- query_ conn "select ST_AsEWKB(geometry)  from osm_trails limit 1"
   --let bs = geometry $ head xs
   --print $ runGet parseGeometry bs 
   --print $ runGet parseLineString bs 
   --print  $ runGet (getByteString 1) $ geometry $ head xs
   print $ (head xs :: TrailTable) 
   return ()
