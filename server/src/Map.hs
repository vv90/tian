module Map where

import Data.Aeson qualified as Aeson
import Generics.SOP qualified as SOP
import Geo (GeoPosition (..), Latitude (..), Longitude (..))
import Language.Haskell.To.Elm (HasElmDecoder, HasElmEncoder, HasElmType)
import Magic.ElmDeriving
import Relude

-- earthCircumference :: Double
-- earthCircumference =
--   40075016.686

-- mapTileSize :: Int
-- mapTileSize = 256

-- data MapTile = MapTile
--   { x :: Int,
--     y :: Int,
--     zoom :: Int
--   }
--   deriving stock (Show, Eq, Generic)
--   deriving anyclass (SOP.Generic, SOP.HasDatatypeInfo, Aeson.ToJSON, Aeson.FromJSON)
--   deriving
--     (HasElmType, HasElmEncoder Aeson.Value, HasElmDecoder Aeson.Value)
--     via ElmType "Api.Map.Tile" MapTile

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

-- toMercatorWeb :: GeoPoint -> (Double, Double)
-- toMercatorWeb (GeoPoint (LatitudeDegrees latDeg) (LongitudeDegrees lonDeg)) =
--   let latRad = latDeg * pi / 180
--       sec x = 1 / cos x
--       resY = logBase (exp 1) (tan latRad + sec latRad)
--    in ( (lonDeg + 180) / 360,
--         (1 - resY / pi) / 2
--       )

-- fromMercatorWeb :: (Double, Double) -> GeoPoint
-- fromMercatorWeb (x, y) =
--   let sinh' a = (exp a - exp (-a)) / 2
--       lonDeg = x * 360 - 180
--       latRad = atan (sinh' (pi * (1 - 2 * y)))
--       latDeg = latRad * 180 / pi
--    in GeoPoint (LatitudeDegrees latDeg) (LongitudeDegrees lonDeg)

-- tileCoords :: MapTile -> GeoPoint
-- tileCoords (MapTile x y zoom) =
--   let n :: Int
--       n = 2 ^ zoom
--    in fromMercatorWeb
--         ( fromIntegral x / fromIntegral n,
--           fromIntegral y / fromIntegral n
--         )

-- data ZoomLevel
--   = Z0
--   | Z1
--   | Z2
--   | Z3
--   | Z4
--   | Z5
--   | Z6
--   | Z7
--   | Z8
--   | Z9
--   | Z10
--   | Z11
--   | Z12
--   | Z13
--   | Z14
--   | Z15
--   | Z16
--   | Z17
--   | Z18
--   | Z19
--   | Z20

-- zoomInt :: ZoomLevel -> Int
-- zoomInt zoom =
--   case zoom of
--     Z0 ->
--       0
--     Z1 ->
--       1
--     Z2 ->
--       2
--     Z3 ->
--       3
--     Z4 ->
--       4
--     Z5 ->
--       5
--     Z6 ->
--       6
--     Z7 ->
--       7
--     Z8 ->
--       8
--     Z9 ->
--       9
--     Z10 ->
--       10
--     Z11 ->
--       11
--     Z12 ->
--       12
--     Z13 ->
--       13
--     Z14 ->
--       14
--     Z15 ->
--       15
--     Z16 ->
--       16
--     Z17 ->
--       17
--     Z18 ->
--       18
--     Z19 ->
--       19
--     Z20 ->
--       20
