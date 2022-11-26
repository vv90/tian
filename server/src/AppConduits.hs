module AppConduits where

import Relude
import TrackPoint (TrackPoint(TrackPoint))
import FlightTrack (FlightTrack(FlightTrack))
import FlightTrack.Parser (buildFlightTrack, FlightInfo(..), flightInfoParser)
import qualified FlightTrack
import qualified TrackPoint
import Conduit (linesUnboundedC, await, ConduitT, ResourceT, yield, sourceFile, decodeUtf8C, mapC, (.|), filterC, runConduitRes, iterateC, printC, stdoutC, sinkNull, leftover, yieldMany, sequenceConduits, ZipConduit (ZipConduit, getZipConduit), sequenceSources)
import Text.Parsec (ParseError, parse, Parsec, many1, alphaNum, char, string)
import TimeUtils (diffTimeToMillis)
import Control.Concurrent (threadDelay)
-- import Data.Conduit.Combinators (linesUnbounded)
import Data.Maybe (fromJust)
-- import Data.Conduit (await)
import Aprs.AprsMessage (AprsMessage (..))
import TaskProgressUtils (TaskState(..), progressInit, progressAdvance)
import FlightTask
import ProgressPoint (ProgressPoint, ProgressPointDto, toDto)
import System.Directory (getDirectoryContents)
import System.FilePath (takeFileName)
import Control.Concurrent.STM (TBQueue, TQueue, writeTQueue, readTQueue)
import Data.Conduit.TQueue (sinkTBQueue, sourceTBQueue, sinkTQueue)

startTimeMillis :: FlightTrack -> Int
startTimeMillis =
    diffTimeToMillis . TrackPoint.time . head . FlightTrack.points 

counter :: Int -> [TrackPoint] -> ConduitT () TrackPoint IO ()
counter time tps =
    let 
        newTime = time + 100

    in
        case tps of
            tp : rest | diffTimeToMillis tp.time >= newTime -> do
                yield tp
                liftIO $ threadDelay 1000000
                counter newTime rest

            tp : rest | otherwise -> do
                liftIO $ threadDelay 1000000 
                counter newTime tps

            [] -> pass



initCounter :: FlightTrack -> ConduitT () TrackPoint IO ()
initCounter flightTrack =
    let 
        tp :| tps = FlightTrack.points flightTrack
        startTime = diffTimeToMillis tp.time
    in
        counter startTime (tp : tps)

filterMapC :: Monad m => (i -> Maybe o) -> ConduitT i o m ()
filterMapC f = 
    await >>= \case
        Just i -> 
            case f i of
                Just o -> yield o >> filterMapC f
                Nothing -> filterMapC f
        Nothing -> pass
    -- let 
    --     go = do
    --         val <- await 
            
    --         case val of 
    --             Just x -> maybe go (yield >>= go) (f x)
                    
    --             Nothing -> pass
    -- in 
    --     go

toAprsMessage :: Text -> FlightInfo -> Maybe AprsMessage
toAprsMessage id fi = 
    case fi of 
        Fix tp -> 
            Just $
                AprsMessage 
                    { source = id
                    , time = tp.time
                    , lat = tp.lat
                    , lon = tp.lon
                    , alt = tp.altitudeGps
                    }

        _ -> Nothing


demoAprsSourceC :: FilePath -> ConduitT () AprsMessage (ResourceT IO) ()
demoAprsSourceC path = 
    sourceFile path 
        .| decodeUtf8C 
        .| linesUnboundedC
        .| filterMapC (rightToMaybe . parse flightInfoParser path)
        .| filterMapC (toAprsMessage $ toText path)

throttledC :: (Monad m, MonadIO m, Show a) => ConduitT a a m ()
throttledC = do
    val <- await
    
    case val of 
        Just x -> do
            liftIO $ threadDelay 1000000
            yield x
            throttledC

        Nothing -> pass

dummyC :: ConduitT () Int (ResourceT IO) ()
dummyC = 
    iterateC succ 1 .| throttledC

rawFileC :: FilePath -> ConduitT () Text (ResourceT IO) ()
rawFileC path = 
    sourceFile path 
        .| decodeUtf8C 
        .| linesUnboundedC

playbackC :: (MonadIO m) => Int -> ConduitT AprsMessage AprsMessage m ()
playbackC speed = 
    let 
        play :: (MonadIO m) => Int -> ConduitT AprsMessage AprsMessage m ()
        play time = do
            item <- await

            case item of 
                Just msg -> do
                    let 
                        newTime = time + (100 * speed)
                        msgTime = diffTimeToMillis msg.time

                    if time >= msgTime then do
                        yield msg
                        liftIO $ threadDelay 100000
                        play newTime
                    else do
                        leftover msg
                        liftIO $ threadDelay 100000
                        play newTime

                Nothing -> pass

    in do
        msg <- await
        
        case msg of 
            Just x -> do
                yield x
                play $ diffTimeToMillis x.time

            Nothing -> pass

progressAdvanceC :: (Monad m) => FlightTask -> TaskState -> ConduitT AprsMessage ProgressPoint m ()
progressAdvanceC ft st = do
    item <- await

    case item of 
        Just msg -> do
            let 
                newSt = progressAdvance ft st msg

            yield $ head newSt.progressPoints

            progressAdvanceC ft newSt

        Nothing -> pass

progressC :: (Monad m) => FlightTask -> ConduitT AprsMessage ProgressPoint m ()
progressC ft = do
    item <- await

    case item of
        Just msg -> do
            let st = progressInit ft msg
            yield $ head st.progressPoints

            progressAdvanceC ft st

        Nothing -> pass


type ProgressDemo = FlightTask -> ConduitT () ProgressPointDto (ResourceT IO) () 

demo :: TBQueue ProgressPointDto -> ProgressDemo
demo queue ft =
    -- let 
    --     c = 
    --         demoAprsSourceC "./demo/155_VB.igc"
    --         .| playbackC 5
    --         .| progressC ft
    --         .| mapC toDto
    --         .| sinkTBQueue queue
    -- in 
        sourceTBQueue queue

sourceQueue :: TQueue ProgressPointDto -> ConduitT () ProgressPointDto (ResourceT IO) ()
sourceQueue queue = 
    do
        x <- liftIO $ atomically $ readTQueue queue
        yield x
        sourceQueue queue

runDemo :: TQueue ProgressPointDto -> FlightTask -> IO ()
runDemo queue ft =
    let 
        sinkQueue :: ConduitT ProgressPointDto Void (ResourceT IO) ()
        sinkQueue = do
            item <- await
            case item of 
                Just x -> do
                    liftIO $ atomically $ writeTQueue queue x
                    sinkQueue
                Nothing ->
                    pass            
    in do
        print "running demo"
        runConduitRes $
            demoAprsSourceC "./demo/155_VB.igc"
                .| playbackC 5
                .| printC
                .| progressC ft
                .| mapC toDto
                .| sinkQueue

-- sinkTracks :: TBQueue ProgressPointDto -> ConduitT ProgressPointDto a (ResourceT IO) ()
-- sinkTracks queue = 
--     sinkTBQueue queue

-- sourceTracks :: TBQueue ProgressPointDto -> ConduitT () ProgressPointDto (ResourceT IO) ()
-- sourceTracks queue = 
--     sourceTBQueue queue

data TrackFileInfo = TrackFileInfo
    { callsign :: String 
    , dateStr :: String
    } 
    deriving (Show)

trackFileInfoParser :: Parsec Text () TrackFileInfo
trackFileInfoParser = 
    TrackFileInfo 
    <$> many1 alphaNum 
    <* char '_'
    <*> many1 alphaNum
    <* string ".igc"

findTrackFiles :: FilePath -> IO [(TrackFileInfo, FilePath)]
findTrackFiles path = 
    let 
        parseTrackFileInfo path = 
            ( ,path) <$> parse trackFileInfoParser "" (toText $ takeFileName path)
    in do
        files <- rightToMaybe . parseTrackFileInfo <<$>> getDirectoryContents path
        print $ 
            show (length $ catMaybes files) 
            <> " recognized track files out of "
            <> show (length files) 
            <> " found"

        pure $ catMaybes files



lettersC :: ConduitT () String (ResourceT IO) ()
lettersC =
   let 
        x = show <$> ['a'..'g']
    in 
    yieldMany x 

numbersC :: ConduitT () String (ResourceT IO) ()
numbersC =
   let 
        x = show <$> [1..3]
    in 
    yieldMany x

testC :: ConduitT () String (ResourceT IO) ()
testC = 
    let 
        c = ZipConduit lettersC <* ZipConduit numbersC
        c1 = sequence_ 
                [ lettersC .| throttledC
                , numbersC .| throttledC
                ]
    in
    c1 -- .| mapC concat

