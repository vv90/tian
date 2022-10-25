
module Geo where

import Relude
import qualified Data.Aeson as Aeson
import qualified Generics.SOP as SOP
import Language.Haskell.To.Elm (HasElmEncoder, HasElmDecoder, HasElmType)
import Magic.ElmDeriving
import Data.Geo.Jord.Geodetic (HorizontalPosition)
import qualified Data.Geo.Jord.Geodetic as Geodetic
import Data.Geo.Jord.Models (S84)

newtype Latitude = LatitudeDegrees Double
    deriving (Show, Read, Eq, Generic, SOP.Generic, SOP.HasDatatypeInfo, Aeson.ToJSON, Aeson.FromJSON)
    deriving (HasElmType, HasElmEncoder Aeson.Value, HasElmDecoder Aeson.Value)
        via ElmType "Api.Geo.Latitude" Latitude

degreesLatitude :: Latitude -> Double
degreesLatitude (LatitudeDegrees x) = x

newtype Longitude = LongitudeDegrees Double
    deriving (Show, Read, Eq, Generic, SOP.Generic, SOP.HasDatatypeInfo, Aeson.ToJSON, Aeson.FromJSON)
    deriving (HasElmType, HasElmEncoder Aeson.Value, HasElmDecoder Aeson.Value)
        via ElmType "Api.Geo.Longitude" Longitude

degreesLongitude :: Longitude -> Double
degreesLongitude (LongitudeDegrees x) = x
newtype Elevation = ElevationMeters Double
    deriving (Show, Read, Eq, Generic, SOP.Generic, SOP.HasDatatypeInfo, Aeson.ToJSON, Aeson.FromJSON)
    deriving (HasElmType, HasElmEncoder Aeson.Value, HasElmDecoder Aeson.Value)
        via ElmType "Api.Geo.Elevation" Elevation

metersElevation :: Elevation -> Double
metersElevation (ElevationMeters x) = x

newtype Direction = DirectionDegrees Int32
    deriving (Show, Read, Eq, Generic, SOP.Generic, SOP.HasDatatypeInfo, Aeson.ToJSON, Aeson.FromJSON)
    deriving (HasElmType, HasElmEncoder Aeson.Value, HasElmDecoder Aeson.Value)
        via ElmType "Api.Geo.Direction" Direction

degreesDirection :: Direction -> Int32
degreesDirection (DirectionDegrees x) = x

newtype Distance = DistanceMeters Double
    deriving (Show, Read, Eq, Generic, SOP.Generic, SOP.HasDatatypeInfo, Aeson.ToJSON, Aeson.FromJSON)
    deriving (HasElmType, HasElmEncoder Aeson.Value, HasElmDecoder Aeson.Value)
        via ElmType "Api.Geo.Distance" Distance

metersDistance :: Distance -> Double
metersDistance (DistanceMeters x) = x


class HasCoordinates a where 
    latitude :: a -> Latitude
    longitude :: a -> Longitude

s84position :: HasCoordinates a => a -> HorizontalPosition S84
s84position pos = 
    let 
        (LatitudeDegrees lat) = latitude pos
        (LongitudeDegrees lon) = longitude pos
    in
    Geodetic.s84Pos lat lon