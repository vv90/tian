module Seed.Utils where

-- import Utils (unflattenVector)

import Control.Concurrent.Async (mapConcurrently_)
import Control.Monad.Except (withExceptT)
import Data.Aeson (encode)
import Data.Aeson.Types ()
import Data.Vector (Vector)
import ElevationPointsTileData (ElevationPointsTile (..))
import Geo (Latitude (..), Longitude (..))
import GeoPoint (GeoPoint (..))
import Hasql.Session qualified as Session
import Mercator (MercatorTileKey (..), ZoomLevel (..), containingTile, tileBoundingGeoPoints, zoomInt)
import Persistence.Connection (getConnection)
import Persistence.Session (generateElevationPointsSession)
import Persistence.Statement (ElevationPointQuery (..))
import Relude
import System.Directory (doesFileExist)
import Utils (writeFileCompressed)

compressFile :: IO ()
compressFile = do
  content <- readFileLBS "./tiles/12/2109_1466.json"
  writeFileCompressed "./tiles/12/2109_1466_compressed.json.gz" content

generateTileElevationPoints :: MercatorTileKey -> IO ()
generateTileElevationPoints tileKey =
  let resolution :: Int
      resolution = 32
      (from, to) = tileBoundingGeoPoints tileKey

      latStep :: Latitude
      latStep = (to.lat - from.lat) / fromIntegral (resolution - 1)

      lonStep :: Longitude
      lonStep = (to.lon - from.lon) / fromIntegral (resolution - 1)

      query = ElevationPointQuery from lonStep latStep resolution

      makeTile :: Vector Int -> ElevationPointsTile
      makeTile = ElevationPointsTile from latStep lonStep resolution

      fileName :: FilePath
      fileName = "./tiles/" <> show tileKey.zoom <> "/" <> show tileKey.x <> "_" <> show tileKey.y <> ".json"

      traverseElevations :: Vector (GeoPoint, Maybe Int) -> Maybe (Vector (GeoPoint, Int))
      traverseElevations = traverse (\(gp, elev) -> fmap (gp,) elev)

      generateTile :: ExceptT String IO ()
      generateTile = do
        conn <- withExceptT show getConnection
        result <-
          withExceptT show
            $ ExceptT
            $ Session.run (generateElevationPointsSession query) conn

        -- when looking up points, elevation may not be available for all points (if the point is outside the loaded dataset for example)
        -- so we need to check if all points are available before writing the tile
        whenJust
          (traverseElevations result)
          (writeFileLBS fileName . encode . makeTile . fmap snd)

      runGenerateTile :: IO ()
      runGenerateTile = do
        result <- runExceptT generateTile
        case result of
          Left err -> putStrLn err
          Right _ -> putTextLn $ show tileKey
   in do
        -- check if tile already exists
        fileExists <- liftIO $ doesFileExist fileName
        unless fileExists runGenerateTile

seed :: IO ()
seed =
  let zoom :: ZoomLevel
      zoom = Z12

      startingTileKey :: MercatorTileKey
      startingTileKey = containingTile zoom $ GeoPoint (LatitudeDegrees 43.0) (LongitudeDegrees 5.0)

      endingTileKey :: MercatorTileKey
      endingTileKey = containingTile zoom $ GeoPoint (LatitudeDegrees 46.0) (LongitudeDegrees 8.0)

      tileKeys = [MercatorTileKey x y (zoomInt zoom) | x <- [startingTileKey.x .. endingTileKey.x], y <- [endingTileKey.y .. startingTileKey.y]]

      -- Define the function to split a list into chunks
      chunk :: Int -> [a] -> [[a]]
      chunk _ [] = []
      chunk n xs = take n xs : chunk n (drop n xs)

      chunks = chunk (length tileKeys `div` 10) tileKeys

      processChunk :: [MercatorTileKey] -> IO ()
      processChunk = traverse_ generateTileElevationPoints
   in do
        mapConcurrently_ processChunk chunks
