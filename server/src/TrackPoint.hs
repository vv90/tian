module TrackPoint where

import Data.Time (DiffTime)
import Geo (Elevation, GeoPosition (..), GeoPosition3d (..), Latitude, Longitude, RecordedGeoPosition (..))
import Relude

data FixValidity
  = Gps3D
  | Baro2D
  deriving stock (Show, Eq)

data TrackPoint = TrackPoint
  { time :: DiffTime,
    lat :: Latitude,
    lon :: Longitude,
    fixValidity :: FixValidity,
    altitudeBaro :: Elevation,
    altitudeGps :: Elevation
  }
  deriving stock (Show, Eq)

instance GeoPosition TrackPoint where
  latitude x = x.lat
  longitude x = x.lon

instance GeoPosition3d TrackPoint where
  altitude x = x.altitudeGps

instance RecordedGeoPosition TrackPoint where
  time x = x.time
