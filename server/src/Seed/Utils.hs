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
      -- query = ElevationPointQuery from lonStep latStep resolution

      makeTile :: Vector Int -> ElevationPointsTile
      makeTile = ElevationPointsTile from latStep lonStep resolution

      fileName :: FilePath
      fileName = "../client/public/tiles/v1/" <> show tileKey.zoom <> "/" <> show tileKey.x <> "_" <> show tileKey.y <> ".json"

      -- traverseElevations :: Vector (GeoPoint, Maybe Int) -> Maybe (Vector (GeoPoint, Int))
      -- traverseElevations = traverse (\(gp, elev) -> fmap (gp,) elev)

      -- lookupElevation :: TiffContents -> GeoPoint -> ExceptT String IO (GeoPoint, Maybe Int)
      -- lookupElevations =
      --   lookupElevationValue contents gp

      -- generateTile :: ExceptT String IO ()
      -- generateTile = do
      --   conn <- withExceptT show getConnection
      --   result <-
      --     withExceptT show
      --       $ ExceptT
      --       $ Session.run (generateElevationPointsSession query) conn

      --   -- when looking up points, elevation may not be available for all points (if the point is outside the loaded dataset for example)
      --   -- so we need to check if all points are available before writing the tile
      --   whenJust
      --     (traverseElevations result)
      --     (writeFileLBS fileName . encode . makeTile . fmap snd)

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
   in --     -- Define the function to split a list into chunks
      --     chunk :: Int -> [a] -> [[a]]
      --     chunk _ [] = []
      --     chunk n xs = take n xs : chunk n (drop n xs)

      --     chunks = chunk (length tileKeys `div` 10) tileKeys

      --     processChunk :: [MercatorTileKey] -> IO ()
      --     processChunk = traverse_ generateTileElevationPoints
      --  in do
      --       mapConcurrently_ processChunk chunks
      do
        elevationsResult <- runExceptT $ readTiffElevationData "./demo/ASTGTMV003_N45E005_dem.tif"

        case elevationsResult of
          Left err -> putStrLn err
          Right elevations -> do
            print $ fst elevations
            traverse_ (generateTileElevationPoints elevations) tileKeys
