module Seed.Utils where

import Control.Monad.Except (withExceptT)
import Data.Aeson (encode)
import Data.Aeson.Types ()
-- import Utils (unflattenVector)
import ElevationPointsTileData (ElevationPointsTile (..))
import Geo (Latitude (..), Longitude (..))
import GeoPoint (GeoPoint (..))
import Hasql.Session qualified as Session
import Mercator (MercatorTileKey (..), tileBoundingGeoPoints)
import Persistence.Connection (getConnection)
import Persistence.Session (generateElevationPointsSession)
import Persistence.Statement (ElevationPointQuery (..))
import Relude

generateTileElevationPoints :: MercatorTileKey -> ExceptT String IO ()
generateTileElevationPoints tileKey =
  let resolution :: Int
      resolution = 32
      (from, to) = tileBoundingGeoPoints tileKey

      latStep :: Latitude
      latStep = (to.lat - from.lat) / fromIntegral (resolution - 1)

      lonStep :: Longitude
      lonStep = (to.lon - from.lon) / fromIntegral (resolution - 1)

      query = ElevationPointQuery from lonStep latStep resolution

      -- asTuple :: (GeoPoint, Int) -> (Int)
      -- asTuple (GeoPoint (LatitudeDegrees lat) (LongitudeDegrees lon), elev) =
      --   (lat, lon, elev)

      makeTile = ElevationPointsTile from latStep lonStep resolution
   in do
        putTextLn $ "generating tile " <> show tileKey
        putTextLn $ "query: " <> show query
        conn <- withExceptT show getConnection
        result <-
          withExceptT show
            $ ExceptT
            $ Session.run (generateElevationPointsSession query) conn

        writeFileLBS ("./tiles/12/" <> show tileKey.x <> "_" <> show tileKey.y <> ".json")
          $ encode
          $ makeTile
          $ fmap snd result

        pass

seed :: ExceptT String IO ()
seed =
  let startX :: Int
      startX = 2109
      -- startX = 2117

      startY :: Int
      startY = 1466

      endX :: Int
      endX = 2116
      -- endX = 2124

      endY :: Int
      endY = 1473

      zoom :: Int
      zoom = 12

      tileKeys = [MercatorTileKey x y zoom | x <- [startX .. endX], y <- [startY .. endY]]
   in do
        traverse_ generateTileElevationPoints tileKeys

-- r <- readTiffElevationData "./demo/ASTGTMV003_N45E005_dem.tif"

-- putStrLn $ "read" ++ show (length r) ++ "point groups"

-- conn <- withExceptT show getConnection
-- done <-
--   withExceptT show
--     $ ExceptT
--     $ Session.run (saveElevationPointsSession "ASTGTMV003_N45E005_dem.tif" r) conn

-- putStrLn $ "added" ++ show done ++ "points"
