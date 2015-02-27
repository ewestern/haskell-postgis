module Postgis.DB where
import Postgis.Types
import Database.PostgreSQL.Simple.FromField
import Data.Serialize
import Postgis.Parser

instance Serialize Geometry where
  get = parseGeometry 
  put = $notImplemented

instance FromField Geometry where
	fromField f m = case m of
    Just bs -> case runGet parseGeometry bs of
                   Left e -> return $ error $ "failed parse" ++ e
                   Right g -> return g 
    Nothing -> error "failed"


