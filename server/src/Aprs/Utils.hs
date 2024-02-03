module Aprs.Utils where

import Aprs.AprsMessage (AprsMessage (..), aprsStreamParser)
import Conduit (ConduitT, await, mapC, runConduit, takeC, (.|))
import Control.Concurrent.STM.TBChan (TBChan, isFullTBChan, readTBChan, writeTBChan)
import Data.Conduit.Network (appSink, appSource, clientSettings, runTCPClient)
import Data.HashMap.Strict as HM
import Data.UUID (UUID)
import Geo (Elevation)
import GeoPoint (GeoPoint (..))
import Relude
import Text.Parsec (parse)

type FlightId = Text

type FlightPosition = (GeoPoint, Elevation)

type AprsMessageBroker = HashMap UUID (TBChan AprsMessage)

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

updateFlightsTable :: TVar AprsMessageBroker -> AprsMessage -> STM ()
updateFlightsTable broker msg = do
  chans <- HM.elems <$> readTVar broker

  traverse_ writeAprsMessage chans
  where
    writeAprsMessage :: TBChan AprsMessage -> STM ()
    writeAprsMessage chan =
      isFullTBChan chan >>= \case
        -- if the channel is full, discard the oldest message
        -- it's better to lose a message than to block the whole system
        True -> readTBChan chan >> writeTBChan chan msg
        False -> writeTBChan chan msg

handleAprsMessage :: TVar AprsMessageBroker -> ConduitT ByteString Void IO ()
handleAprsMessage broker =
  await >>= \case
    Nothing -> pass
    Just message ->
      case parse aprsStreamParser "" message of
        Right msgs -> do
          atomically $ traverse_ distributeMessage msgs
          handleAprsMessage broker
        Left _ -> do
          -- liftIO $ appendFileBS errorLogFile message
          handleAprsMessage broker
  where
    distributeMessage :: AprsMessage -> STM ()
    distributeMessage msg = do
      chans <- HM.elems <$> readTVar broker
      traverse_ (writeAprsMessage msg) chans

    writeAprsMessage :: AprsMessage -> TBChan AprsMessage -> STM ()
    writeAprsMessage msg chan =
      isFullTBChan chan >>= \case
        -- if the channel is full, discard the oldest message
        -- it's better to lose a message than to block the whole system
        True -> readTBChan chan >> writeTBChan chan msg
        False -> writeTBChan chan msg

runAprs :: TVar AprsMessageBroker -> IO ()
runAprs broker =
  runTCPClient (clientSettings 14580 "aprs.glidernet.org") $ \server -> do
    -- errorLogFileExists <- doesFileExist errorLogFile
    -- unless errorLogFileExists $ writeFile errorLogFile ""
    runConduit $ appSource server .| withAuth (handleAprsMessage broker) (appSink server)
