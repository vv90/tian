module Aprs.Utils where

import Aprs.AprsMessage (AprsMessage (..), DeviceId, aprsMessageParser, getDeviceId)
import Aprs.GlidernetId (AircraftType)
import Aprs.LocationDatapoint (fromAprsMessage)
import Backend.FlightsState (FlightInformation (..), FlightPosition (..), lookupFlightInformation, toFlightPosition)
import Codec.Compression.GZip (CompressParams (compressLevel))
import Codec.Compression.GZip qualified as GZip
import Conduit (ConduitT, await, runConduit, yield, (.|))
import Control.Concurrent (threadDelay)
import Control.Concurrent.Async (concurrently_)
import Control.Concurrent.STM.TBChan (TBChan, isFullTBChan, readTBChan, writeTBChan)
import Data.Aeson qualified as Aeson
import Data.Conduit.Combinators (linesUnboundedAscii)
import Data.Conduit.Network (AppData, appSink, appSource, clientSettings, runTCPClient)
import Data.HashMap.Strict qualified as HM
import Data.Time (UTCTime (utctDay), getCurrentTime, utctDayTime)
import Data.UUID (UUID)
import Glidernet.DeviceDatabase (DeviceInfo)
import Relude
import System.Directory (createDirectoryIfMissing, doesDirectoryExist, listDirectory, removeDirectory, removeFile)
import System.FilePath (takeExtension, (<.>), (</>))
import Text.Parsec (parse)
import TimeUtils (diffTimeToSeconds)

type AprsMessageBroker = HashMap UUID (TBChan (DeviceId, FlightPosition))

type FlightsState = HashMap DeviceId (FlightInformation, FlightPosition)

-- errorLogFile :: FilePath
-- errorLogFile = "aprs-error.log"

logsDirectory :: FilePath
logsDirectory = "logs"

authC :: ConduitT a ByteString IO ()
authC =
  yield "user N0CALLX09 pass -1 vers TaskView 0.1 filter r/45.2/5.72/1000\r\n"

withAuth :: ConduitT ByteString Void IO () -> ConduitT ByteString Void IO () -> ConduitT ByteString Void IO ()
withAuth messageSink responseSink = do
  authC .| responseSink
  putStrLn "authenticated"
  messageSink

isPositionRecentEnough :: Int -> FlightPosition -> Bool
isPositionRecentEnough currTimeSeconds (FlightPosition {timeSeconds}) =
  let timeDiffSeconds :: Int
      timeDiffSeconds = currTimeSeconds - timeSeconds -- negative time difference means the position is from the previous day
   in timeDiffSeconds < 1800 && timeDiffSeconds >= 0 -- less than 30 minutes old and on the same day

compressFiles :: FilePath -> IO ()
compressFiles dirName =
  let compressionParams :: CompressParams
      compressionParams = GZip.defaultCompressParams {compressLevel = GZip.bestCompression}

      dirPath :: FilePath
      dirPath = logsDirectory </> dirName

      archivePath :: FilePath
      archivePath = logsDirectory </> "archive" </> dirName

      compressAndRemove :: FilePath -> IO ()
      compressAndRemove fileName =
        let filePath :: FilePath
            filePath = dirPath </> fileName
         in do
              contents <- readFileLBS filePath
              createDirectoryIfMissing True archivePath
              writeFileLBS (archivePath </> fileName <.> "gz") $ GZip.compressWith compressionParams contents
              removeFile filePath
   in do
        files <- filter ((== ".json") . takeExtension) <$> listDirectory dirPath
        traverse_ compressAndRemove files

        leftover <- listDirectory dirPath
        when (null leftover) $ removeDirectory dirPath

cleanUpFlightsState :: TVar FlightsState -> IO ()
cleanUpFlightsState flights = do
  todaysDirectoryName <- show . utctDay <$> getCurrentTime
  dirs <- listDirectory logsDirectory >>= filterM (\dir -> doesDirectoryExist (logsDirectory </> dir))

  traverse_ compressFiles (filter (/= todaysDirectoryName) dirs)

  currTimeSeconds <- round . diffTimeToSeconds . utctDayTime <$> getCurrentTime
  atomically $ modifyTVar' flights (HM.filter (isPositionRecentEnough currTimeSeconds . snd))

saveMessageJson :: AircraftType -> AprsMessage -> IO ()
saveMessageJson aircraftType msg = do
  currDate <- getCurrentTime
  let dirPath = logsDirectory </> show (utctDay currDate)
  createDirectoryIfMissing True dirPath
  let filePath = dirPath </> toString (getDeviceId msg.source) <.> show aircraftType <.> "json"
  appendFileLBS filePath (Aeson.encode $ fromAprsMessage msg)

runAprs :: HashMap Text DeviceInfo -> TVar AprsMessageBroker -> TVar FlightsState -> IO ()
runAprs devices broker flights =
  let runAprsClient :: IO ()
      runAprsClient = runTCPClient (clientSettings 14580 "aprs.glidernet.org") (runConduit . aprsServerConduit)

      aprsServerConduit :: AppData -> ConduitT () Void IO ()
      aprsServerConduit server =
        appSource server .| withAuth (linesUnboundedAscii .| processLines) (appSink server)

      processLines :: ConduitT ByteString Void IO ()
      processLines = do
        line <- await
        case parse aprsMessageParser "" <$> line of
          Just (Right msg) -> do
            -- liftIO $ putText "." >> hFlush stdout

            let flightInfo :: FlightInformation
                flightInfo = lookupFlightInformation devices msg

            atomically $ distributeMessage flightInfo msg
            _ <- liftIO $ saveMessageJson flightInfo.aircraftType msg

            processLines
          Just (Left _) -> do
            -- liftIO $ putText "x" >> hFlush stdout
            -- pass
            -- whenJust line (liftIO . appendFileBS errorLogFile)
            processLines
          Nothing ->
            putStrLn "APRS connection closed"

      distributeMessage :: FlightInformation -> AprsMessage -> STM ()
      distributeMessage flightInfo msg =
        let position :: FlightPosition
            position = toFlightPosition msg
         in do
              modifyTVar' flights $ HM.insert msg.source (flightInfo, position)
              chans <- HM.elems <$> readTVar broker
              traverse_ (writeMessage (msg.source, position)) chans

      writeMessage :: (DeviceId, FlightPosition) -> TBChan (DeviceId, FlightPosition) -> STM ()
      writeMessage msg chan =
        isFullTBChan chan >>= \case
          -- if the channel is full, discard the oldest message
          -- it's better to lose a message than to block the whole system
          True -> readTBChan chan >> writeTBChan chan msg
          False -> writeTBChan chan msg
   in -- errorLogFileExists <- doesFileExist errorLogFile
      -- unless errorLogFileExists $ writeFile errorLogFile ""
      concurrently_
        (infinitely runAprsClient)
        (infinitely $ threadDelay 10000000 >> cleanUpFlightsState flights)
