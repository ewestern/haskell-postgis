
module Database.Postgis
  (
    module Database.Postgis.Geometry
  , readGeometry
  , writeGeometry

  ) where
import Database.Postgis.Geometry (
          SRID,
          Point(..),
          LineString(..),
          LinearRing,
          Polygon(..),
          MultiPoint(..),
          MultiLineString(..),
          MultiPolygon(..),
          Geometry(..)
        )
import Database.Postgis.Serialize (
        writeGeometry,
        readGeometry
      )


