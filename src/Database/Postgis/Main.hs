
module Database.Postgis
  (

  )
import Database.Postgis.Simple.Types
import Database.PostgreSQL.Simple.FromField
import Data.Serialize.Get
import Postgis.Parser

instance FromField Geometry where
	fromField f m = case m of
    Just bs -> case runGet get bs of
                   Left e -> return $ error $ "failed parse" ++ e
                   Right g -> return g 
    Nothing -> error "failed"

