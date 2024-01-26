module Seed.Utils where

import Data.Aeson (encode)
import Data.Aeson.Types ()
import Data.Vector (Vector)
import Data.Vector qualified as Vector
import ElevationPointsTileData (ElevationPointsTile (..))
import Geo (Latitude (..), Longitude (..))
import GeoPoint (GeoPoint (..))
import GeoTiff.Tiff (TiffContents, lookupElevationValue, readTiffElevationData)
import Mercator (MercatorTileKey (..), containingTile, tileBoundingGeoPoints)
import Relude
import System.Directory (doesFileExist)
import Utils (writeFileCompressed)

compressFile :: IO ()
compressFile = do
  content <- readFileLBS "./tiles/12/2109_1466.json"
  writeFileCompressed "./tiles/12/2109_1466_compressed.json.gz" content

generateTileElevationPoints :: TiffContents -> MercatorTileKey -> IO ()
generateTileElevationPoints tiffContents tileKey =
  let resolution :: Int
      resolution = 32
      (from, to) = tileBoundingGeoPoints tileKey

      latStep :: Latitude
      latStep = (to.lat - from.lat) / fromIntegral (resolution - 1)

      lonStep :: Longitude
      lonStep = (to.lon - from.lon) / fromIntegral (resolution - 1)

      geoPoints :: [GeoPoint]
      geoPoints =
        [ GeoPoint
            (from.lat + (latStep * fromIntegral y))
            (from.lon + (lonStep * fromIntegral x))
          | y <- [0 .. resolution - 1],
            x <- [0 .. resolution - 1]
        ]

      makeTile :: Vector Int -> ElevationPointsTile
      makeTile = ElevationPointsTile from latStep lonStep resolution

      fileName :: FilePath
      fileName = "../client/public/tiles/v1/" <> show tileKey.zoom <> "/" <> show tileKey.x <> "_" <> show tileKey.y <> ".json"

      runGenerateTile :: IO ()
      runGenerateTile = do
        whenJust
          (traverse (lookupElevationValue tiffContents) geoPoints)
          (writeFileLBS fileName . encode . makeTile . Vector.fromList . fmap fromIntegral)
   in do
        -- check if tile already exists
        fileExists <- liftIO $ doesFileExist fileName
        unless fileExists runGenerateTile

seed :: IO ()
seed =
  let startingTileKey :: MercatorTileKey
      startingTileKey = containingTile 12 $ GeoPoint (LatitudeDegrees 45.0) (LongitudeDegrees 5.0)

      endingTileKey :: MercatorTileKey
      endingTileKey = containingTile 12 $ GeoPoint (LatitudeDegrees 46.0) (LongitudeDegrees 6.0)

      tileKeys =
        [ MercatorTileKey x y 12
          | x <- [startingTileKey.x .. endingTileKey.x],
            y <- [endingTileKey.y .. startingTileKey.y]
        ]
  in do
    elevationsResult <- runExceptT $ readTiffElevationData "./demo/ASTGTMV003_N45E005_dem.tif"

    case elevationsResult of
      Left err -> putStrLn err
      Right elevations -> do
        print $ fst elevations
        traverse_ (generateTileElevationPoints elevations) tileKeys
