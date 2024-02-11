module Aprs.Utils where

import Aprs.AprsMessage (AprsMessage (..), DeviceId, aprsMessageParser)
import Backend.FlightsState (FlightInformation, FlightPosition (..), makeFlightInformation, toFlightPosition)
import Conduit (ConduitT, awaitForever, mapC, runConduit, takeC, (.|))
import Control.Concurrent (threadDelay)
import Control.Concurrent.Async (concurrently_)
import Control.Concurrent.STM.TBChan (TBChan, isFullTBChan, readTBChan, writeTBChan)
import Data.Conduit.Combinators (linesUnboundedAscii)
import Data.Conduit.Network (appSink, appSource, clientSettings, runTCPClient)
import Data.HashMap.Strict as HM
import Data.Time (getCurrentTime, utctDayTime)
import Data.UUID (UUID)
import Glidernet.DeviceDatabase (DeviceInfo)
import Relude
import Text.Parsec (parse)
import TimeUtils (diffTimeToSeconds)

-- type FlightId = Text

-- type FlightPosition = (GeoPoint, Elevation)

type AprsMessageBroker = HashMap UUID (TBChan (DeviceId, FlightPosition))

type FlightsState = HashMap DeviceId (FlightInformation, FlightPosition)

-- errorLogFile :: FilePath
-- errorLogFile = "aprs-error.log"

authC :: ConduitT a ByteString IO ()
authC =
  mapC (const "user N0CALLX09 pass -1 vers TaskView 0.1 filter r/45.2/5.8/1000\r\n")

withAuth :: ConduitT ByteString Void IO () -> ConduitT ByteString Void IO () -> ConduitT ByteString Void IO ()
withAuth ptSink responseSink = do
  takeC 1 .| authC .| responseSink
  putStrLn "authenticated"
  ptSink

handleAprsMessage :: HashMap Text DeviceInfo -> TVar AprsMessageBroker -> TVar FlightsState -> ConduitT ByteString Void IO ()
handleAprsMessage devices broker flights =
  linesUnboundedAscii .| awaitForever processLine
  where
    processLine :: ByteString -> ConduitT ByteString Void IO ()
    processLine line =
      case parse aprsMessageParser "" line of
        Right msg -> do
          -- liftIO $ putText "."
          atomically $ distributeMessage msg
        Left _ -> do
          -- liftIO $ putText "x"
          pass
    -- liftIO $ appendFileBS errorLogFile line--(line <> "\n" <> show err <> "\n")

    distributeMessage :: AprsMessage -> STM ()
    distributeMessage msg =
      let information :: FlightInformation
          information = makeFlightInformation devices msg

          position :: FlightPosition
          position = toFlightPosition msg
       in do
            modifyTVar' flights $ HM.insert msg.source (information, position)
            chans <- HM.elems <$> readTVar broker
            traverse_ (writeMessage (msg.source, position)) chans

    writeMessage :: (DeviceId, FlightPosition) -> TBChan (DeviceId, FlightPosition) -> STM ()
    writeMessage msg chan =
      isFullTBChan chan >>= \case
        -- if the channel is full, discard the oldest message
        -- it's better to lose a message than to block the whole system
        True -> readTBChan chan >> writeTBChan chan msg
        False -> writeTBChan chan msg

isPositionRecentEnough :: Int -> FlightPosition -> Bool
isPositionRecentEnough currTimeSeconds (FlightPosition {timeSeconds}) =
  let timeDiffSeconds :: Int
      timeDiffSeconds = currTimeSeconds - timeSeconds -- negative time difference means the position is from the previous day
   in timeDiffSeconds < 1800 && timeDiffSeconds >= 0 -- less than 30 minutes old and on the same day

cleanUpFlightsState :: TVar FlightsState -> IO ()
cleanUpFlightsState flights = do
  currTimeSeconds <- round . diffTimeToSeconds . utctDayTime <$> getCurrentTime
  atomically $ modifyTVar' flights (HM.filter (isPositionRecentEnough currTimeSeconds . snd))

runAprs :: HashMap Text DeviceInfo -> TVar AprsMessageBroker -> TVar FlightsState -> IO ()
runAprs devices broker flights =
  runTCPClient (clientSettings 14580 "aprs.glidernet.org") $ \server -> do
    -- errorLogFileExists <- doesFileExist errorLogFile
    -- unless errorLogFileExists $ writeFile errorLogFile ""
    concurrently_
      (runConduit $ appSource server .| withAuth (handleAprsMessage devices broker flights) (appSink server))
      (infinitely $ threadDelay 10000000 >> cleanUpFlightsState flights)
