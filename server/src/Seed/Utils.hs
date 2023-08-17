module Seed.Utils where

import Control.Monad.Except (withExceptT)
import Hasql.Session qualified as Session
import Persistence.Connection (getConnection)
import Persistence.Session (saveElevationPointsSession)
import GeoTiff.Tiff (readTiffElevationData)
import Relude

seed :: ExceptT String IO ()
seed = do
  r <- readTiffElevationData "./demo/ASTGTMV003_N45E005_dem.tif"

  putStrLn $ "read" ++ show (length r) ++ "point groups"

  conn <- withExceptT show getConnection
  done <-
    withExceptT show
      $ ExceptT
      $ Session.run (saveElevationPointsSession "ASTGTMV003_N45E005_dem.tif" r) conn

  putStrLn $ "added" ++ show done ++ "points"