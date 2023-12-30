module Aprs.Utils where

import Aprs.AprsMessage (AprsMessage (..), aprsMessageParser)
import Conduit (ConduitT, await, mapC, runConduit, takeC, (.|))
import Control.Concurrent.STM.TBChan (TBChan, newTBChan, writeTBChan)
import Data.Conduit.Network (appSink, appSource, clientSettings, runTCPClient)
import Data.HashMap.Strict as HM
import Geo (Elevation, altitude, latitude, longitude)
import Map (GeoPoint (..))
import Relude
import Text.Parsec (parse)

type FlightId = Text

type FlightPosition = (GeoPoint, Elevation)

type FlightsTable = HashMap FlightId (TBChan FlightPosition)

fromAprsMessage :: AprsMessage -> FlightPosition
fromAprsMessage msg = (GeoPoint (latitude msg) (longitude msg), altitude msg)

authC :: ConduitT ByteString ByteString IO ()
authC =
  mapC (const "user N0CALLX09 pass -1 vers TaskView 0.1 filter r/45.2/5.8/1000\r\n")

withAuth :: ConduitT ByteString Void IO () -> ConduitT ByteString Void IO () -> ConduitT ByteString Void IO ()
withAuth ptSink responseSink = do
  takeC 1 .| authC .| responseSink
  putStrLn "authenticated"
  ptSink

updateFlightsTable :: TVar FlightsTable -> AprsMessage -> STM ()
updateFlightsTable flightsTvar msg = do
  flight <- HM.lookup (source msg) <$> readTVar flightsTvar

  case flight of
    Nothing -> do
      chan <- newTBChan 1000
      modifyTVar' flightsTvar $ HM.insert (source msg) chan
      writeTBChan chan $ fromAprsMessage msg
    Just chan -> do
      writeTBChan chan $ fromAprsMessage msg

handleAprsMessage :: TVar FlightsTable -> ConduitT ByteString Void IO ()
handleAprsMessage flightsTvar = do
  message <- await

  case parse aprsMessageParser "" <$> message of
    Nothing -> pass
    Just (Right msg) -> do
      -- liftIO $ putStrLn "."
      atomically $ updateFlightsTable flightsTvar msg
      -- readTVarIO flightsTvar >>= print . sort . HM.keys

      handleAprsMessage flightsTvar
    Just (Left _) -> do
      -- liftIO $ putStrLn "X"
      handleAprsMessage flightsTvar

runAprs :: TVar FlightsTable -> IO ()
runAprs flightsTvar = runTCPClient (clientSettings 14580 "aprs.glidernet.org") $ \server -> do
  runConduit $ appSource server .| withAuth (handleAprsMessage flightsTvar) (appSink server)
