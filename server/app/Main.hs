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
import AppConduits (testC, runDemo)
import Control.Concurrent.STM (newTBQueue, newTBQueueIO, TBQueue, TMVar, newEmptyTMVar, TQueue, newTQueue)
import ProgressPoint (ProgressPointDto(ProgressPointDto))
import FlightTask (FlightTask)
import Env (checkRequiredEnvironmentKeys)

startCounter :: Int -> IO ()
startCounter n = do
    print n
    threadDelay 1000000
    startCounter (n + 1)

-- runMain :: IO ()
-- runMain =
--     concurrently startApp

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

-- buildFlightTrackC = do


-- trackFileC =
--     runConduitRes 
--     $ sourceFile "demo/155_VB.igc"
--     .| decodeUtf8C
--     .| linesUnbounded
--     -- .| 

-- parseFile :: Parsec Text () a -> String -> ByteString -> Either String a
-- parseFile parser name = 
--     left show . parse parser name . decodeUtf8
 
-- loadFlightTrack :: FilePath -> IO (Either String FlightTrack)
-- loadFlightTrack path = 
--     (parseFile flightInfoParser path >=> buildFlightTrack) <$> readFileBS path

-- startTimeMillis :: [FlightTrack] -> Maybe Int
-- startTimeMillis fts =
--     viaNonEmpty minimum1 
--     $ diffTimeToMillis . TrackPoint.time . head . FlightTrack.points 
--     <$> fts 

-- runDemo = 
--     runConduitRes $ 
--         demoAprsSource "./demo/155_VB.igc"
--         .| printC

-- runTest = 
--     runConduitRes $ 
--         testC
--         .| printC

demoThread :: TQueue (String, ProgressPointDto) -> TMVar FlightTask -> IO ()
demoThread queue var = do
    threadDelay 100000
    x <- atomically $ tryReadTMVar var
    case x of
        Just ft ->
            runDemo queue ft
        Nothing ->
            demoThread queue var

main :: IO ()
main = do 
    checkRequiredEnvironmentKeys
    queue <- atomically (newTQueue :: STM (TQueue (String, ProgressPointDto)))
    var <- atomically (newEmptyTMVar :: STM (TMVar FlightTask))
    -- track <- loadFile
    -- flightTracks <- getDirectoryContents "./demo" >>= mapM loadFlightTrack . filter (isExtensionOf ".igc")
    concurrently_ (startApp 8081 queue var) (demoThread queue var)
    -- startApp 8081 (demo queue) (runDemo queue)
    
