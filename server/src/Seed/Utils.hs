module Seed.Utils where

import Control.Monad.Except (withExceptT)
import Hasql.Session qualified as Session
import Persistence.Connection (getConnection)
import Persistence.Session (saveElevationPointsSession)
import Relude

seedPointNemo :: ExceptT Text IO ()
seedPointNemo = do
  conn <- getConnection
  _done <-
    withExceptT show
      $ ExceptT
      $ Session.run (saveElevationPointsSession (0 :: Int32, 0.0 :: Double, 0 :: Double)) conn

  putStrLn "added point nemo"
