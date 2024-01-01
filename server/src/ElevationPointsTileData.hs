module ElevationPointsTileData where

import Data.Aeson qualified as Aeson
import Data.Vector (Vector)
import Generics.SOP qualified as SOP
import Geo (Latitude, Longitude)
import GeoPoint (GeoPoint)
import Language.Haskell.To.Elm (HasElmDecoder, HasElmEncoder, HasElmType)
import Magic.ElmDeriving (ElmType)
import Relude

data ElevationPointsTile = ElevationPointsTile
  { origin :: GeoPoint,
    latStep :: Latitude,
    lonStep :: Longitude,
    rowLength :: Int,
    elevations :: Vector Int
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (SOP.Generic, SOP.HasDatatypeInfo, Aeson.ToJSON, Aeson.FromJSON)
  deriving
    (HasElmType, HasElmEncoder Aeson.Value, HasElmDecoder Aeson.Value)
    via ElmType "Api.Types.ElevationPointsTile" ElevationPointsTile
