module Seed.Utils where

import Control.Concurrent.Async (mapConcurrently)
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

generateTileElevationPoints :: [TiffContents] -> MercatorTileKey -> IO ()
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

      lookupElevationInFiles :: GeoPoint -> Maybe Word16
      lookupElevationInFiles geoPoint =
        asum $ fmap (`lookupElevationValue` geoPoint) tiffContents

      runGenerateTile :: IO ()
      runGenerateTile = do
        whenJust
          (traverse lookupElevationInFiles geoPoints)
          (writeFileLBS fileName . encode . makeTile . Vector.fromList . fmap fromIntegral)
   in do
        -- check if tile already exists
        fileExists <- liftIO $ doesFileExist fileName
        unless fileExists runGenerateTile

seed :: IO ()
seed =
  let startingTileKey :: Int -> MercatorTileKey
      startingTileKey zoom = containingTile zoom $ GeoPoint (LatitudeDegrees 43.0) (LongitudeDegrees 5.0)

      endingTileKey :: Int -> MercatorTileKey
      endingTileKey zoom = containingTile zoom $ GeoPoint (LatitudeDegrees 46.0) (LongitudeDegrees 8.0)

      getX :: MercatorTileKey -> Int
      getX = x

      getY :: MercatorTileKey -> Int
      getY = y

      demFiles :: [FilePath]
      demFiles =
        [ "./demo/ASTGTMV003_N43E005_dem.tif",
          "./demo/ASTGTMV003_N43E006_dem.tif",
          "./demo/ASTGTMV003_N43E007_dem.tif",
          "./demo/ASTGTMV003_N44E005_dem.tif",
          "./demo/ASTGTMV003_N44E006_dem.tif",
          "./demo/ASTGTMV003_N44E007_dem.tif",
          "./demo/ASTGTMV003_N45E005_dem.tif",
          "./demo/ASTGTMV003_N45E006_dem.tif",
          "./demo/ASTGTMV003_N45E007_dem.tif"
        ]

      tileKeys =
        [ MercatorTileKey x y z
          | z <- [12, 13, 14, 15],
            x <- [getX (startingTileKey z) .. getX (endingTileKey z)],
            y <- [getY (endingTileKey z) .. getY (startingTileKey z)]
        ]
   in do
        -- elevationsResult <- runExceptT $ readTiffElevationData "./demo/ASTGTMV003_N45E005_dem.tif"

        -- elevationsResult' <- runExceptT $ traverse readTiffElevationData demFiles

        elevationsResult <- sequence <$> mapConcurrently (runExceptT . readTiffElevationData) demFiles

        case elevationsResult of
          Left err -> putStrLn err
          Right elevations -> do
            putStrLn "Data loaded! Generating tiles..."
            traverse_ (generateTileElevationPoints elevations) tileKeys
