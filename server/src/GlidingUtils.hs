module GlidingUtils where

import Data.Aeson qualified as Aeson
import Generics.SOP qualified as SOP
import Geo (Elevation (..), GeoPosition (..), GeoPosition3d (..), Latitude, Longitude, MovingGeoPosition (..), RecordedGeoPosition (..), metersElevation, metersPerSecondSpeed)
import Language.Haskell.To.Elm (HasElmDecoder, HasElmEncoder, HasElmType)
import Magic.ElmDeriving (ElmType)
import Relude
import TimeUtils (diffTimeToSeconds)

data TotalEnergyPoint = TotalEnergyPoint
  { lat :: Latitude,
    lon :: Longitude,
    alt :: Elevation,
    tod :: Int,
    energy :: Double
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (SOP.Generic, SOP.HasDatatypeInfo, Aeson.ToJSON, Aeson.FromJSON)
  deriving
    (HasElmType, HasElmEncoder Aeson.Value, HasElmDecoder Aeson.Value)
    via ElmType "Api.Types.TotalEnergyPoint" TotalEnergyPoint

calculateTotalEnergy :: (GeoPosition3d a, RecordedGeoPosition a, MovingGeoPosition a) => a -> TotalEnergyPoint
calculateTotalEnergy dataPoint =
  let altMeters = metersElevation $ altitude dataPoint
      speedMetersPerSecond = metersPerSecondSpeed $ speed dataPoint
   in TotalEnergyPoint
        { lat = latitude dataPoint,
          lon = longitude dataPoint,
          alt = altitude dataPoint,
          tod = round $ diffTimeToSeconds $ time dataPoint,
          energy = (altMeters * 9.8) + ((speedMetersPerSecond ^ (2 :: Integer)) * 0.5)
        }
