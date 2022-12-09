module Demo.DemoConduit where

import Relude
import Text.Parsec (Parsec, many1, alphaNum, char, string, parse)
import System.FilePath (takeFileName)
import System.Directory (getDirectoryContents)
import Conduit (ConduitT, decodeUtf8LenientC, (.|), linesUnboundedC, mapOutputMaybe, sinkNull, ResourceT, runConduitRes, await, printC, mapC, yield, sourceDirectory, leftover, takeWhileC, takeC, headC, peekC, dropC, yieldMany, bracketP)
import Aprs.AprsMessage (AprsMessage(..))
import FlightTrack.Parser (FlightInfo (..), flightInfoParser)
import TrackPoint (TrackPoint(..))
import Data.Conduit.Combinators (sourceFile)
import Data.Time (DiffTime)
import TimeUtils (millisToDiffTime, diffTimeToMillis)
import Control.Concurrent (threadDelay, forkIO, killThread)
import Control.Concurrent.STM.TBMQueue (TBMQueue, newTBMQueueIO, newTBMQueue, readTBMQueue, peekTBMQueue, closeTBMQueue)
import Data.Conduit (sequenceSources)
import Data.Semigroup (Min(Min, getMin))
import Data.Conduit.Internal (ConduitT(..))
import Data.Conduit.TQueue (sinkTBMQueue)
import Control.Concurrent.STM (TBQueue, newTBQueue)
import Control.Concurrent.Async (mapConcurrently_, concurrently_, withAsync, wait)
import FlightTask (FlightTask)
import TaskProgressUtils (TaskState(progressPoints), progressAdvance, progressInit)
import ProgressPoint (ProgressPoint, toDto, ProgressPointDto(time))

data TrackFileInfo = TrackFileInfo
    { dateStr :: Text
    , callsign :: Text 
    } 
    deriving (Show)

trackFileInfoParser :: Parsec Text () TrackFileInfo
trackFileInfoParser = 
    TrackFileInfo 
    <$> (toText <$> many1 alphaNum)
    <* char '_'
    <*> (toText <$> many1 alphaNum)
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
            <> " recognized track files"

        pure $ catMaybes files

demoAprsSource :: TrackFileInfo -> FilePath -> ConduitT () AprsMessage (ResourceT IO) ()
demoAprsSource info path = do
    sourceFile path
        .| decodeUtf8LenientC
        .| mapOutputMaybe 
            (toFlightInfo (show path) >=> toAprsMessage info.callsign) 
            linesUnboundedC
    where
        toFlightInfo :: Text -> Text -> Maybe FlightInfo
        toFlightInfo id =
            rightToMaybe . parse flightInfoParser (show id)

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

playbackC :: (MonadIO m) =>(a -> Int) -> Int -> ConduitT a a m ()
playbackC getTime speed = 
    let 
        stepMillis = 100

        play time =
            whenJustM await $ \msg -> 
                if time >= getTime msg
                then do
                    yield msg
                    play time
                else do
                    leftover msg
                    liftIO $ threadDelay (1000 * stepMillis) 
                    play $ time + (stepMillis * speed)
    in
    whenJustM await $ \msg -> 
        yield msg >> play (getTime msg)

extractSmallest :: (Ord b) => (a -> b) -> NonEmpty a -> (a, [a])
extractSmallest f (x :| xs) =
    let 
        extract :: (Ord b) => (a -> b) -> [a] -> a -> [a] -> (a, [a])
        extract f [] x xs = 
            (x, xs)

        extract f (y:ys) x xs =
            if f y < f x
            then extract f ys y (x:xs)
            else extract f ys x (y:xs)
    in 
        extract f xs x []


mergeSourcesOn :: (Ord b) => (a -> b) -> [ConduitT () a (ResourceT IO) ()] -> ConduitT () a (ResourceT IO) ()
mergeSourcesOn f sources = 
    let 
        makeSinkQueue c = do
            q <- atomically $ newTBMQueue 100
            pure (q, c)
        
        feedSinkQueue q c =
            runConduitRes $ 
                bracketP 
                    pass
                    (\_ -> atomically $ closeTBMQueue q)
                    (\_ -> c .| sinkTBMQueue q)


        readQ q = do
            v <- atomically $ readTBMQueue q
            pure $ (,q) <$> v
        
        consumeQueues qs = do
            let m = viaNonEmpty (extractSmallest (\(v, q) -> f v)) qs
            whenJust m $ \((v, q), rest) -> do
                yield v
                next <- readQ q
                case next of
                    Just x -> do
                        consumeQueues $ x : rest
                    Nothing -> do
                        consumeQueues rest
    in do
        qs <- traverse (liftIO . makeSinkQueue) sources

        bracketP 
            (liftIO $ traverse (forkIO . uncurry feedSinkQueue) qs)
            (liftIO . traverse_ killThread)
            (\tids -> do 
                qs' <- catMaybes <$> traverse readQ (fst <$> qs)
                consumeQueues qs'
            )

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


demoC :: FlightTask -> ConduitT () (Text, ProgressPointDto) (ResourceT IO) ()
demoC ft = 
    let 
        toProgressSource (info, p) = 
            demoAprsSource info ("./demo/" <> p) 
                .| progressC ft
                .| mapC (\p -> (info.callsign, toDto p))
    in do
    sources <- liftIO $ toProgressSource <<$>> findTrackFiles "./demo/" 

    mergeSourcesOn (\(_, p) -> p.time) sources
        .| playbackC (\(_, p) -> p.time) 10


testDemoConduit :: IO ()
testDemoConduit =
    let 
        toAprsSource (info, p) = 
            demoAprsSource info ("./demo/" <> p)
        r =  toAprsSource <<$>> findTrackFiles "./demo/"
    in do
        sources <- toAprsSource <<$>> findTrackFiles "./demo/"
        runConduitRes $ 
        --     -- demoC "./demo/"
        --     -- .| mapC (\xs -> (\x -> (x.source, x.time)) <<$>> xs)
        --     -- .| printC
            -- takeAprsMsg (demoAprsSource (TrackFileInfo "" "SO") "./demo/155_SO.igc")
            mergeSourcesOn (\m -> m.time) sources
           
            -- .| playbackC 10
            .| mapC (\x -> (x.source, x.time))
            .| printC

    
        