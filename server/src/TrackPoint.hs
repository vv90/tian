module TrackPoint where 

import Relude
import Data.Time (DiffTime)
import Geo (Latitude, Longitude, Elevation, GeoPosition (..))

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
    latitude = lat
    longitude = lon
