module GeoTiff.ElevationPoint where

import Relude
import Geo (Longitude(..), Latitude(..), GeoPosition(..))

data ElevationPoint = ElevationPoint 
  { elevByte :: Int16 
  , lon :: Longitude
  , lat :: Latitude
  } deriving stock (Show, Eq)

instance GeoPosition ElevationPoint where
  longitude = lon
  latitude = lat