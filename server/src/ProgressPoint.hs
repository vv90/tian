{-# LANGUAGE MultiParamTypeClasses #-}

module ProgressPoint where

import Data.Aeson qualified as Aeson
import Data.Time (DiffTime)
import Generics.SOP qualified as SOP
import Geo (Distance, Elevation, GeoPosition (..), GeoPosition3d (..), Latitude, Longitude, RecordedGeoPosition (..), metersDistance)
import Language.Elm.Expression qualified as Expression
import Language.Haskell.To.Elm (HasElmDecoder (elmDecoder), HasElmEncoder (elmEncoder), HasElmType (elmType))
import Magic.ElmDeriving
import NavPoint (NavPoint, name)
import Relude
import Relude.Extra (bimapF)
import TimeUtils (diffTimeToMillis)

-- instance HasElmType DiffTime where
--     elmType = "Time.Posix"

-- instance HasElmEncoder Aeson.Value DiffTime where
--     elmEncoder = "Json.Encode.int" Expression.<< "Time.posixToMillis"

-- instance HasElmDecoder Aeson.Value DiffTime where
--     elmDecoder = Expression.apps "Json.Decode.map" ["Time.millisToPosix", "Json.Decode.int"]

data ProgressPoint = ProgressPoint
  { time :: DiffTime,
    lat :: Latitude,
    lon :: Longitude,
    altitude :: Elevation,
    -- , target :: Maybe (NavPoint, Distance)
    target :: Maybe NavPoint,
    distance :: Distance,
    speed :: Maybe Double
  }
  deriving (Show, Eq)

instance GeoPosition ProgressPoint where
  latitude x = x.lat
  longitude x = x.lon

instance GeoPosition3d ProgressPoint where
  altitude x = x.altitude

instance RecordedGeoPosition ProgressPoint where
  time x = x.time

data ProgressPointDto = ProgressPointDto
  { time :: Int,
    lat :: Latitude,
    lon :: Longitude,
    altitude :: Elevation,
    -- , target :: Maybe (Text, Distance)
    target :: Maybe Text,
    distance :: Double,
    speed :: Maybe Double
  }
  deriving (Show, Read, Eq, Generic, SOP.Generic, SOP.HasDatatypeInfo, Aeson.ToJSON, Aeson.FromJSON)
  deriving
    (HasElmType, HasElmEncoder Aeson.Value, HasElmDecoder Aeson.Value)
    via ElmType "Api.TaskProgress.ProgressPoint" ProgressPointDto

toDto :: ProgressPoint -> ProgressPointDto
toDto (ProgressPoint time' lat' lon' altitude' target' distance' speed') =
  ProgressPointDto
    { time = diffTimeToMillis time',
      lat = lat',
      lon = lon',
      altitude = altitude',
      target = name <$> target',
      -- , target = bimapF name id target'
      distance = metersDistance distance',
      speed = speed'
    }
