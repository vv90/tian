module GeoTiff.ElevationPoint where

import Geo (GeoPosition (..), Latitude (..), Longitude (..))
import Relude

data ElevationPoint = ElevationPoint
  { elevByte :: Int16,
    lon :: Longitude,
    lat :: Latitude
  }
  deriving stock (Show, Eq)

instance GeoPosition ElevationPoint where
  longitude = lon
  latitude = lat
