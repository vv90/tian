module Backend.Utils where

import Aprs.Utils (runAprs)
import Control.Concurrent.Async (concurrently_)
import Data.HashMap.Strict as HM
import Glidernet.DeviceDatabase (DeviceInfo (..), DeviceType (..), deviceDatabaseParser)
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

    deviceTypePrefix :: DeviceType -> Text
    deviceTypePrefix Flarm = "FLR"
    deviceTypePrefix OGN = "OGN"
    deviceTypePrefix ICAO = "ICA"

    makeDeviceDict :: [DeviceInfo] -> HashMap Text DeviceInfo
    makeDeviceDict = HM.fromList . fmapToFst (\info -> deviceTypePrefix info.deviceType <> info.deviceId)

runBackend :: IO ()
runBackend = do
  deviceDict <- getDeviceDict
  broker <- newTVarIO HM.empty
  flights <- newTVarIO HM.empty
  concurrently_ (startApp broker flights 8081) (runAprs deviceDict broker flights)
