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
import Postgis.DB

data Table = Table {
  geometry :: Geometry
}
instance FromRow Table where
  fromRow = Table <$> field
```

