module Map where

import Relude
import qualified Data.Aeson as Aeson
import Geo (Latitude(..), Longitude(..))
import qualified Generics.SOP as SOP
import Language.Haskell.To.Elm (HasElmEncoder, HasElmDecoder, HasElmType)
import Magic.ElmDeriving

earthCircumference :: Double
earthCircumference =
    40075016.686

mapTileSize :: Int
mapTileSize = 256

data MapTile = MapTile 
    { x :: Int
    , y :: Int
    , zoom :: Int 
    }
    deriving (Show, Eq, Generic, SOP.Generic, SOP.HasDatatypeInfo, Aeson.ToJSON, Aeson.FromJSON)
    deriving (HasElmType, HasElmEncoder Aeson.Value, HasElmDecoder Aeson.Value)
        via ElmType "Api.Map.Tile" MapTile

type GeoPoint = (Latitude, Longitude)

toMercatorWeb :: GeoPoint -> (Double, Double)
toMercatorWeb (LatitudeDegrees latDeg, LongitudeDegrees lonDeg) =
    let 
        latRad = latDeg * pi / 180
        sec x = 1 / cos x
        resY = logBase (exp 1) (tan latRad + sec latRad)
    in
        ( (lonDeg + 180) / 360
        , (1 - resY / pi) / 2
        )

fromMercatorWeb :: (Double, Double) -> GeoPoint
fromMercatorWeb (x, y) =
    let 
        sinh a = (exp a - exp (-a)) / 2
        lonDeg = x * 360 - 180
        latRad = atan (sinh (pi * (1 - 2 * y)))
        latDeg = latRad * 180 / pi
    in
        (LatitudeDegrees latDeg, LongitudeDegrees lonDeg)

tileCoords :: MapTile -> GeoPoint
tileCoords (MapTile x y zoom) =
    let 
        n = 2 ^ zoom
    in
    fromMercatorWeb 
        ( fromIntegral x / fromIntegral n
        , fromIntegral y / fromIntegral n
        )