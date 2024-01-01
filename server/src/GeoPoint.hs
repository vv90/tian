module GeoPoint where

import Data.Aeson qualified as Aeson
import Generics.SOP qualified as SOP
import Geo (GeoPosition (..), Latitude (..), Longitude (..))
import Language.Haskell.To.Elm (HasElmDecoder, HasElmEncoder, HasElmType)
import Magic.ElmDeriving (ElmType)
import Relude

data GeoPoint = GeoPoint
  { lat :: Latitude,
    lon :: Longitude
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (SOP.Generic, SOP.HasDatatypeInfo, Aeson.ToJSON, Aeson.FromJSON)
  deriving
    (HasElmType, HasElmEncoder Aeson.Value, HasElmDecoder Aeson.Value)
    via ElmType "Api.Map.GeoPoint" GeoPoint

instance GeoPosition GeoPoint where
  latitude :: GeoPoint -> Latitude
  latitude p = p.lat

  longitude :: GeoPoint -> Longitude
  longitude p = p.lon
