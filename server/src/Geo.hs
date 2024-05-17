module Geo where

import Data.Aeson qualified as Aeson
import Data.Geo.Jord.Geodetic (HorizontalPosition)
import Data.Geo.Jord.Geodetic qualified as Geodetic
import Data.Time (DiffTime)
import Generics.SOP qualified as SOP
import Language.Haskell.To.Elm (HasElmDecoder, HasElmEncoder, HasElmType)
import Magic.ElmDeriving
import Relude

newtype Latitude = LatitudeDegrees Double
  deriving stock (Show, Read, Eq, Generic)
  deriving anyclass (SOP.Generic, SOP.HasDatatypeInfo, Aeson.ToJSON, Aeson.FromJSON)
  deriving
    (HasElmType, HasElmEncoder Aeson.Value, HasElmDecoder Aeson.Value)
    via ElmType "Api.Types.Latitude" Latitude

degreesLatitude :: Latitude -> Double
degreesLatitude (LatitudeDegrees x) = x

instance Num Latitude where
  (+) :: Latitude -> Latitude -> Latitude
  (LatitudeDegrees x) + (LatitudeDegrees y) = LatitudeDegrees (x + y)
  (-) :: Latitude -> Latitude -> Latitude
  (LatitudeDegrees x) - (LatitudeDegrees y) = LatitudeDegrees (x - y)
  (*) :: Latitude -> Latitude -> Latitude
  (LatitudeDegrees x) * (LatitudeDegrees y) = LatitudeDegrees (x * y)
  abs :: Latitude -> Latitude
  abs (LatitudeDegrees x) = LatitudeDegrees (abs x)
  signum :: Latitude -> Latitude
  signum (LatitudeDegrees x) = LatitudeDegrees (signum x)
  fromInteger :: Integer -> Latitude
  fromInteger x = LatitudeDegrees (fromInteger x)

instance Fractional Latitude where
  (/) :: Latitude -> Latitude -> Latitude
  (LatitudeDegrees x) / (LatitudeDegrees y) = LatitudeDegrees (x / y)

  fromRational :: Rational -> Latitude
  fromRational x = LatitudeDegrees (fromRational x)

newtype Longitude = LongitudeDegrees Double
  deriving stock (Show, Read, Eq, Generic)
  deriving anyclass (SOP.Generic, SOP.HasDatatypeInfo, Aeson.ToJSON, Aeson.FromJSON)
  deriving
    (HasElmType, HasElmEncoder Aeson.Value, HasElmDecoder Aeson.Value)
    via ElmType "Api.Types.Longitude" Longitude

degreesLongitude :: Longitude -> Double
degreesLongitude (LongitudeDegrees x) = x

instance Num Longitude where
  (+) :: Longitude -> Longitude -> Longitude
  (LongitudeDegrees x) + (LongitudeDegrees y) = LongitudeDegrees (x + y)
  (-) :: Longitude -> Longitude -> Longitude
  (LongitudeDegrees x) - (LongitudeDegrees y) = LongitudeDegrees (x - y)
  (*) :: Longitude -> Longitude -> Longitude
  (LongitudeDegrees x) * (LongitudeDegrees y) = LongitudeDegrees (x * y)
  abs :: Longitude -> Longitude
  abs (LongitudeDegrees x) = LongitudeDegrees (abs x)
  signum :: Longitude -> Longitude
  signum (LongitudeDegrees x) = LongitudeDegrees (signum x)
  fromInteger :: Integer -> Longitude
  fromInteger x = LongitudeDegrees (fromInteger x)

instance Fractional Longitude where
  (/) :: Longitude -> Longitude -> Longitude
  (LongitudeDegrees x) / (LongitudeDegrees y) = LongitudeDegrees (x / y)

  fromRational :: Rational -> Longitude
  fromRational x = LongitudeDegrees (fromRational x)

newtype Elevation = ElevationMeters Double
  deriving stock (Show, Read, Eq, Generic)
  deriving anyclass (SOP.Generic, SOP.HasDatatypeInfo, Aeson.ToJSON, Aeson.FromJSON)
  deriving
    (HasElmType, HasElmEncoder Aeson.Value, HasElmDecoder Aeson.Value)
    via ElmType "Api.Types.Elevation" Elevation

elevationFeet :: Double -> Elevation
elevationFeet x = ElevationMeters (x * 0.3048)

metersElevation :: Elevation -> Double
metersElevation (ElevationMeters x) = x

newtype Direction = DirectionDegrees Int32
  deriving stock (Show, Read, Eq, Generic)
  deriving anyclass (SOP.Generic, SOP.HasDatatypeInfo, Aeson.ToJSON, Aeson.FromJSON)
  deriving
    (HasElmType, HasElmEncoder Aeson.Value, HasElmDecoder Aeson.Value)
    via ElmType "Api.Types.Direction" Direction

degreesDirection :: Direction -> Int32
degreesDirection (DirectionDegrees x) = x

newtype Distance = DistanceMeters Double
  deriving stock (Show, Read, Eq, Generic)
  deriving anyclass (SOP.Generic, SOP.HasDatatypeInfo, Aeson.ToJSON, Aeson.FromJSON)
  deriving
    (HasElmType, HasElmEncoder Aeson.Value, HasElmDecoder Aeson.Value)
    via ElmType "Api.Types.Distance" Distance

metersDistance :: Distance -> Double
metersDistance (DistanceMeters x) = x

newtype Speed = SpeedMetersPerSecond Double
  deriving stock (Show, Read, Eq, Generic)
  deriving anyclass (SOP.Generic, SOP.HasDatatypeInfo, Aeson.ToJSON, Aeson.FromJSON)
  deriving
    (HasElmType, HasElmEncoder Aeson.Value, HasElmDecoder Aeson.Value)
    via ElmType "Api.Types.Speed" Speed

metersPerSecondSpeed :: Speed -> Double
metersPerSecondSpeed (SpeedMetersPerSecond x) = x

newtype AngularSpeed = DegreesPerSecond Double
  deriving stock (Show, Read, Eq, Generic)
  deriving anyclass (SOP.Generic, SOP.HasDatatypeInfo, Aeson.ToJSON, Aeson.FromJSON)
  deriving
    (HasElmType, HasElmEncoder Aeson.Value, HasElmDecoder Aeson.Value)
    via ElmType "Api.Types.AngularSpeed" AngularSpeed

metersPerSecondAngularSpeed :: AngularSpeed -> Double
metersPerSecondAngularSpeed (DegreesPerSecond x) = x

class GeoPosition a where
  latitude :: a -> Latitude
  longitude :: a -> Longitude

class (GeoPosition a) => GeoPosition3d a where
  altitude :: a -> Elevation

class (GeoPosition a) => RecordedGeoPosition a where
  time :: a -> DiffTime

class (GeoPosition a) => MovingGeoPosition a where
  speed :: a -> Speed
  heading :: a -> Direction

instance GeoPosition (HorizontalPosition a) where
  latitude = LatitudeDegrees . Geodetic.decimalLatitude
  longitude = LongitudeDegrees . Geodetic.decimalLongitude

roundN :: (RealFrac a) => Int -> a -> a
roundN n x = ((fromIntegral @Integer) . round $ x * f) / f
  where
    f :: (RealFrac a) => a
    f = 10 ^ n

-- Degrees Decimal Minutes (DDM) to Decimal Degrees (DD)
ddmTodd :: (RealFrac b) => Int -> b -> b
ddmTodd deg minutes =
  -- assuming decMin will have at most 3 digits
  -- the maximum precision of the input is 0.001' ~ 0.000017°, so we round to 6 decimal places
  -- (roundN @_ @Integer) 6 $ fromIntegral deg + fromIntegral minutes / 60 + fromIntegral decMin / 60000
  fromIntegral deg + minutes / 60
