module Backend.Utils where

import Aprs.Utils (runAprs)
import Control.Concurrent.Async (concurrently_)
import Data.HashMap.Strict as HM
import Lib (startApp)
import Relude

runBackend :: IO ()
runBackend = do
  flightsTvar <- newTVarIO HM.empty

  concurrently_ (startApp 8081) (runAprs flightsTvar)
  pass
