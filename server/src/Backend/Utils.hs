module Backend.Utils where

import Aprs.Utils (runAprs)
import Control.Concurrent.Async (concurrently_)
import Data.HashMap.Strict as HM
import Glidernet.DeviceDatabase (DeviceInfo (..), deviceDatabaseParser)
import Lib (startApp)
import Network.HTTP.Simple (Response, getResponseBody, httpBS)
import Relude
import Relude.Extra (fmapToFst)
import Text.Parsec (ParseError, parse)

getDeviceDict :: IO (HashMap Text DeviceInfo)
getDeviceDict = do
  response <- httpBS "https://ddb.glidernet.org/download/"
  case parseDeviceDatabase response of
    Right x -> pure $ makeDeviceDict x
    Left e -> error $ show e
  where
    parseDeviceDatabase :: Response ByteString -> Either ParseError [DeviceInfo]
    parseDeviceDatabase =
      parse deviceDatabaseParser "" . getResponseBody

    makeDeviceDict :: [DeviceInfo] -> HashMap Text DeviceInfo
    makeDeviceDict = HM.fromList . fmapToFst deviceId

runBackend :: IO ()
runBackend = do
  deviceDict <- getDeviceDict
  broker <- newTVarIO HM.empty
  flights <- newTVarIO HM.empty
  concurrently_ (startApp deviceDict broker 8081) (runAprs deviceDict broker flights)
