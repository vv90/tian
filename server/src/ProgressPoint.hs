{-# LANGUAGE MultiParamTypeClasses #-}

module ProgressPoint where

import Relude
import Data.Time (DiffTime)
import Geo (Latitude, Longitude, Elevation, Distance, GeoPosition (..))
import NavPoint (NavPoint, name)

import qualified Data.Aeson as Aeson
import qualified Generics.SOP as SOP
import Language.Haskell.To.Elm (HasElmEncoder (elmEncoder), HasElmDecoder (elmDecoder), HasElmType (elmType))
import Magic.ElmDeriving
import qualified Language.Elm.Expression as Expression
import Relude.Extra (bimapF)

-- instance HasElmType DiffTime where
--     elmType = "Time.Posix"

-- instance HasElmEncoder Aeson.Value DiffTime where
--     elmEncoder = "Json.Encode.int" Expression.<< "Time.posixToMillis"

-- instance HasElmDecoder Aeson.Value DiffTime where
--     elmDecoder = Expression.apps "Json.Decode.map" ["Time.millisToPosix", "Json.Decode.int"]

data ProgressPoint = ProgressPoint
    { time :: Int
    , lat :: Latitude
    , lon :: Longitude
    , altitude :: Elevation
    , target :: Maybe (NavPoint, Distance)
    }

instance GeoPosition ProgressPoint where
    latitude = lat
    longitude = lon

data ProgressPointDto = ProgressPointDto
    { time :: Int
    , lat :: Latitude
    , lon :: Longitude
    , altitude :: Elevation
    , target :: Maybe (Text, Distance)
    }
    deriving (Show, Read, Eq, Generic, SOP.Generic, SOP.HasDatatypeInfo, Aeson.ToJSON, Aeson.FromJSON)
    deriving (HasElmType, HasElmEncoder Aeson.Value, HasElmDecoder Aeson.Value)
        via ElmType "Api.TaskProgress.ProgressPoint" ProgressPointDto

toDto :: ProgressPoint -> ProgressPointDto
toDto (ProgressPoint time' lat' lon' altitude' target') = ProgressPointDto
    { time = time'
    , lat = lat'
    , lon = lon'
    , altitude = altitude'
    , target = bimapF name id target'
    }