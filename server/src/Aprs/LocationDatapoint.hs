module Aprs.LocationDatapoint where

import Aprs.AprsMessage (AprsMessage)
import Aprs.AprsMessage qualified as Aprs
import Data.Aeson qualified as Aeson
import Data.Time (secondsToDiffTime)
import Geo
  ( AngularSpeed (..),
    Direction,
    Elevation (..),
    GeoPosition (..),
    GeoPosition3d (..),
    Latitude,
    Longitude,
    MovingGeoPosition (..),
    RecordedGeoPosition (..),
    Speed (..),
    degreesLatitude,
    degreesLongitude,
    metersElevation,
    metersPerSecondAngularSpeed,
    metersPerSecondSpeed,
    roundN,
  )
import Relude

data LocationDatapoint = LocationDatapoint
  { timeSeconds :: Int,
    lat :: Latitude,
    lon :: Longitude,
    heading :: Direction,
    speed :: Speed,
    elev :: Elevation,
    verticalSpeed :: Maybe Speed,
    rotation :: Maybe AngularSpeed
  }
  deriving stock (Show, Eq, Generic)

fromAprsMessage :: AprsMessage -> LocationDatapoint
fromAprsMessage message =
  LocationDatapoint
    { timeSeconds = Aprs.timeSeconds message,
      lat = Aprs.lat message,
      lon = Aprs.lon message,
      heading = Aprs.heading message,
      speed = Aprs.speed message,
      elev = Aprs.elev message,
      verticalSpeed = Aprs.verticalSpeed message,
      rotation = Aprs.rotation message
    }

instance Aeson.ToJSON LocationDatapoint where
  toJSON x =
    Aeson.toJSON
      ( timeSeconds x,
        roundN 4 $ degreesLatitude $ lat x,
        roundN 4 $ degreesLongitude $ lon x,
        x.heading,
        roundN 4 $ metersPerSecondSpeed x.speed,
        roundN 4 $ metersElevation $ elev x,
        roundN 4 . metersPerSecondSpeed <$> verticalSpeed x,
        roundN 4 . metersPerSecondAngularSpeed <$> rotation x
      )

instance Aeson.FromJSON LocationDatapoint where
  parseJSON = Aeson.withArray "LocationDatapoint" $ \arr ->
    case toList arr of
      [timeSeconds, lat, lon, heading', speed', elev, verticalSpeed, rotation] ->
        LocationDatapoint <$> Aeson.parseJSON timeSeconds <*> Aeson.parseJSON lat <*> Aeson.parseJSON lon <*> Aeson.parseJSON heading' <*> Aeson.parseJSON speed' <*> Aeson.parseJSON elev <*> Aeson.parseJSON verticalSpeed <*> Aeson.parseJSON rotation
      _ -> fail "Expected array of 8 elements"

instance GeoPosition LocationDatapoint where
  latitude x = x.lat
  longitude x = x.lon

instance GeoPosition3d LocationDatapoint where
  altitude :: LocationDatapoint -> Elevation
  altitude x = x.elev

instance RecordedGeoPosition LocationDatapoint where
  time = secondsToDiffTime . fromIntegral . timeSeconds

instance MovingGeoPosition LocationDatapoint where
  speed :: LocationDatapoint -> Speed
  speed x = x.speed

  heading :: LocationDatapoint -> Direction
  heading x = x.heading
