module Main where

import Relude
import Lib ( startApp )
import Control.Concurrent.Async (concurrently, concurrently_)
import Control.Concurrent (threadDelay)
import Data.Conduit.Network (runTCPClient, clientSettings, appSource, appSink, AppData)
import Conduit (stdoutC, runConduit, takeC, mapC, runConduitRes, sourceFile, decodeUtf8C, omapCE, sinkNull, ResourceT, printC)
import Data.Conduit ((.|), ConduitT, yield)
import System.Directory (getCurrentDirectory, getDirectoryContents)
import Data.Conduit.Combinators (linesUnbounded)
import Text.Parsec (parse, Parsec)
import FlightTrack.Parser (flightInfoParser, buildFlightTrack, FlightInfo)
import FlightTrack (FlightTrack(FlightTrack))
import TrackPoint (TrackPoint(TrackPoint))
import Control.Arrow (left)
import System.FilePath (isExtensionOf)
import Data.Time (DiffTime)
import qualified TrackPoint (time)
import qualified FlightTrack (points)
import TimeUtils (diffTimeToSeconds, diffTimeToMillis)
import Relude.Extra (Foldable1(minimum1))
import Text.Parsec.Error (ParseError)
import Control.Concurrent.STM (newTBQueue, newTBQueueIO, TBQueue, TMVar, newEmptyTMVar, TQueue, newTQueue, newBroadcastTChan)
import ProgressPoint (ProgressPointDto(ProgressPointDto))
import FlightTask (FlightTask)
import Env (checkRequiredEnvironmentKeys)
import Data.Conduit.TMChan (TMChan, newBroadcastTMChan, dupTMChan)
import Demo.DemoConduit (testDemoConduit)
import GeoTiff.Tiff (convertTiffSafe, testGeoTiff, convertTiff)

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
    
