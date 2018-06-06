# Haskell-Postgis
A collection of types and parsers to use with the PostGIS extesion to PostgreSQL.

## Installation
```
git clone https://github.com/ewestern/haskell-postgis.git
cd haskell-postgis
cabal install
```

## Usage
```
import Database.PostgreSQL.Simple.ToRow
import Database.PostgreSQL.Simple.ToField
import Database.PostgreSQL.Simple.FromRow
import Database.PostgreSQL.Simple.FromField 
import Database.Postgis

data Table = Table {
  geometry :: Geometry
}
instance FromRow Table where
  fromRow = Table <$> field

instance ToRow Table where
  toRow (Table g)  = [toField g]

instance ToField Geometry where
  toField  =  Plain . fromByteString . writeGeometry 

instance FromField Geometry where
	fromField f m = case m of
              Just bs -> return $ readGeometry bs
              Nothing -> error "Invalid Field" 

```

