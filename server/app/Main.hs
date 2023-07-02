module Main where

import Conduit (ResourceT, decodeUtf8C, mapC, omapCE, printC, runConduit, runConduitRes, sinkNull, sourceFile, stdoutC, takeC)
import Control.Arrow (left)
import Control.Concurrent (threadDelay)
import Control.Concurrent.Async (concurrently, concurrently_)
import Control.Concurrent.STM (TBQueue, TMVar, TQueue, newBroadcastTChan, newEmptyTMVar, newTBQueue, newTBQueueIO, newTQueue)
import Data.Conduit (ConduitT, yield, (.|))
import Data.Conduit.Combinators (linesUnbounded)
import Data.Conduit.Network (AppData, appSink, appSource, clientSettings, runTCPClient)
import Data.Conduit.TMChan (TMChan, dupTMChan, newBroadcastTMChan)
import Data.Time (DiffTime)
import Demo.DemoConduit (testDemoConduit)
import Env (checkRequiredEnvironmentKeys)
import FlightTask (FlightTask)
import FlightTrack (FlightTrack (FlightTrack))
import FlightTrack qualified (points)
import FlightTrack.Parser (FlightInfo, buildFlightTrack, flightInfoParser)
import GeoTiff.Tiff (convertTiff, convertTiffSafe, testGeoTiff)
import Lib (startApp)
import ProgressPoint (ProgressPointDto (ProgressPointDto))
import Relude
import Relude.Extra (Foldable1 (minimum1))
import System.Directory (getCurrentDirectory, getDirectoryContents)
import System.FilePath (isExtensionOf)
import Text.Parsec (Parsec, parse)
import Text.Parsec.Error (ParseError)
import TimeUtils (diffTimeToMillis, diffTimeToSeconds)
import TrackPoint (TrackPoint (TrackPoint))
import TrackPoint qualified (time)

startCounter :: Int -> IO ()
startCounter n = do
  print n
  threadDelay 1000000
  startCounter (n + 1)

authC :: ConduitT ByteString ByteString IO ()
authC =
  mapC (const "user N0CALLX09 pass -1 vers TaskView 0.1 filter r/43.7/5.8/500\r\n")

appC :: ConduitT ByteString Void IO () -> ConduitT ByteString Void IO () -> ConduitT ByteString Void IO ()
appC ptSink responseSink = do
  takeC 1 .| authC .| responseSink
  print "authenticated"
  ptSink

runAprs = runTCPClient (clientSettings 14580 "aprs.glidernet.org") $ \server -> do
  runConduit $ appSource server .| appC stdoutC (appSink server)

main :: IO ()
main = do
  print "starting server..."
  checkRequiredEnvironmentKeys
  -- print "N52E039"
  -- convertTiffSafe "./demo/ASTGTMV003_N52E039_dem.tif"

  -- print "N51E039"
  -- convertTiffSafe "./demo/ASTGTMV003_N51E039_dem.tif"

  -- convertTiffSafe "./demo/ASTGTMV003_N44E006_dem.tif"
  -- convertTiffSafe "./demo/ASTGTMV003_N45E005_dem.tif"

  -- print "done"
  startApp 8081

-- convertTiffSafe "./demo/ASTGTMV003_N45E005_dem.tif"

-- mapM print d
-- pure ()
