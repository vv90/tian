module GeoTiff.ElevationPoint where

import Geo (GeoPosition (..), Latitude (..), Longitude (..))
import Relude

data ElevationPoint = ElevationPoint
  { elevByte :: Int16, -- elevation in meters above sea level rounded to the nearest meter and stored as a 2-byte signed integer
    lon :: Longitude,
    lat :: Latitude
  }
  deriving stock (Show, Eq)

instance GeoPosition ElevationPoint where
  longitude = lon
  latitude = lat
