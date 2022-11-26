module TrackPoint where 

import Relude
import Data.Time (DiffTime)
import Geo (Latitude, Longitude, Elevation, GeoPosition (..), GeoPosition3d (..), RecordedGeoPosition (..))

data FixValidity
    = Gps3D
    | Baro2D
    deriving (Show, Eq)

data TrackPoint = TrackPoint
    { time :: DiffTime
    , lat :: Latitude
    , lon :: Longitude
    , fixValidity :: FixValidity
    , altitudeBaro :: Elevation
    , altitudeGps :: Elevation
    } deriving (Show, Eq)

instance GeoPosition TrackPoint where
    latitude x = x.lat
    longitude x = x.lon

instance GeoPosition3d TrackPoint where
    altitude x = x.altitudeGps

instance RecordedGeoPosition TrackPoint where
    time x = x.time