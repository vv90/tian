module Geo where

import Data.Aeson qualified as Aeson
import Data.Geo.Jord.Geodetic (HorizontalPosition)
import Data.Geo.Jord.Geodetic qualified as Geodetic
import Data.Geo.Jord.Models (S84)
import Data.Time (DiffTime)
import Generics.SOP qualified as SOP
import Language.Haskell.To.Elm (HasElmDecoder, HasElmEncoder, HasElmType)
import Magic.ElmDeriving
import Relude

newtype Latitude = LatitudeDegrees Double
  deriving (Show, Read, Eq, Generic, SOP.Generic, SOP.HasDatatypeInfo, Aeson.ToJSON, Aeson.FromJSON)
  deriving
    (HasElmType, HasElmEncoder Aeson.Value, HasElmDecoder Aeson.Value)
    via ElmType "Api.Geo.Latitude" Latitude

degreesLatitude :: Latitude -> Double
degreesLatitude (LatitudeDegrees x) = x

newtype Longitude = LongitudeDegrees Double
  deriving (Show, Read, Eq, Generic, SOP.Generic, SOP.HasDatatypeInfo, Aeson.ToJSON, Aeson.FromJSON)
  deriving
    (HasElmType, HasElmEncoder Aeson.Value, HasElmDecoder Aeson.Value)
    via ElmType "Api.Geo.Longitude" Longitude

degreesLongitude :: Longitude -> Double
degreesLongitude (LongitudeDegrees x) = x

newtype Elevation = ElevationMeters Double
  deriving (Show, Read, Eq, Generic, SOP.Generic, SOP.HasDatatypeInfo, Aeson.ToJSON, Aeson.FromJSON)
  deriving
    (HasElmType, HasElmEncoder Aeson.Value, HasElmDecoder Aeson.Value)
    via ElmType "Api.Geo.Elevation" Elevation

metersElevation :: Elevation -> Double
metersElevation (ElevationMeters x) = x

newtype Direction = DirectionDegrees Int32
  deriving (Show, Read, Eq, Generic, SOP.Generic, SOP.HasDatatypeInfo, Aeson.ToJSON, Aeson.FromJSON)
  deriving
    (HasElmType, HasElmEncoder Aeson.Value, HasElmDecoder Aeson.Value)
    via ElmType "Api.Geo.Direction" Direction

degreesDirection :: Direction -> Int32
degreesDirection (DirectionDegrees x) = x

newtype Distance = DistanceMeters Double
  deriving (Show, Read, Eq, Generic, SOP.Generic, SOP.HasDatatypeInfo, Aeson.ToJSON, Aeson.FromJSON)
  deriving
    (HasElmType, HasElmEncoder Aeson.Value, HasElmDecoder Aeson.Value)
    via ElmType "Api.Geo.Distance" Distance

metersDistance :: Distance -> Double
metersDistance (DistanceMeters x) = x

class GeoPosition a where
  latitude :: a -> Latitude
  longitude :: a -> Longitude

class (GeoPosition a) => GeoPosition3d a where
  altitude :: a -> Elevation

class (GeoPosition a) => RecordedGeoPosition a where
  time :: a -> DiffTime

instance GeoPosition (HorizontalPosition a) where
  latitude = LatitudeDegrees . Geodetic.decimalLatitude
  longitude = LongitudeDegrees . Geodetic.decimalLongitude

roundN :: (RealFrac a, Integral b) => b -> a -> a
roundN n x = (fromIntegral . round $ x * f) / f
  where
    f = 10 ^ n

-- Degrees Decimal Minutes (DDM) to Decimal Degrees (DD)
ddmTodd :: (RealFrac b) => Int -> Int -> Int -> b
ddmTodd deg minutes decMin =
  -- since the maximum precision of the input is 0.001' ~ 0.000017° we round to 6 decimal places
  roundN 6 $ fromIntegral deg + fromIntegral minutes / 60 + fromIntegral decMin / 60000
