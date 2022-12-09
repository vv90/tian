
module Demo.Playback where

import Relude
import qualified Text.Show
import Control.Arrow (left)
import FlightTrack (FlightTrack)
import qualified FlightTrack (points)
import FlightTrack.Parser (flightInfoParserAll, buildFlightTrack)
import qualified TrackPoint (time)
import Text.Parsec.Combinator (many1)
import Text.Parsec.Char (alphaNum)
import Text.Parsec (Parsec, parse, string, char)
import System.FilePath (takeFileName)
import System.Directory (getDirectoryContents)
import TimeUtils (diffTimeToMillis)
import TrackPoint (TrackPoint)
import ProgressPoint (ProgressPointDto(..), ProgressPoint, toDto)
import Entity (Entity(..))
import FlightTask (FlightTask)
import TaskProgressUtils (progress)
import TaskProgress (TaskProgress(TaskProgress, points))
import Control.Concurrent (threadDelay)
import Control.Concurrent.Async (mapConcurrently_, mapConcurrently, concurrently_)
import Data.Semigroup (Max(Max, getMax), Min (getMin, Min))
import Control.Monad.Reader (mapReaderT)
import Data.Map.Strict (assocs)
import Data.Aeson (fromJSON, decode, decode', eitherDecodeFileStrict')
import Control.Monad.Except (withExceptT)
import Demo.DemoTask (loadDemoTask)
import Control.Concurrent.STM (readTQueue, TQueue, TChan, readTChan, writeTChan, writeTQueue, dupTChan, newBroadcastTChan)
import Data.Conduit.TMChan (TMChan, writeTMChan, readTMChan, dupTMChan)


data TrackFileInfo = TrackFileInfo
    { dateStr :: String
    , callsign :: String 
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
            <> " recognized track files found"

        pure $ catMaybes files


parseFile :: Parsec Text () a -> String -> ByteString -> Either String a
parseFile parser name = 
    left show . parse parser name . decodeUtf8

loadFlightTrack :: FilePath -> ExceptT String IO FlightTrack
loadFlightTrack path = 
    ExceptT $ (parseFile flightInfoParserAll path >=> buildFlightTrack path) <$> readFileBS path

loadFlightTask :: FilePath -> ExceptT String IO FlightTask
loadFlightTask path =
    ExceptT $ eitherDecodeFileStrict' path

startTime :: [FlightTrack] -> Int
startTime fts =
    let 
        starts = diffTimeToMillis . TrackPoint.time . head . FlightTrack.points <$> fts
    in 
        getMin $ foldMap Min starts

endTime :: [FlightTrack] -> Int
endTime fts =
    let 
        ends = diffTimeToMillis . TrackPoint.time . last . FlightTrack.points <$> fts
    in 
        getMax $ foldMap Max ends

-- play :: TQueue (String, ProgressPointDto) -> String -> [ProgressPointDto] -> IO ()
-- play queue id allPoints =
    

--     _
data DemoEnv = DemoEnv
    { var :: TMVar (String, ProgressPointDto)
    , start :: Int
    , end :: Int
    , playSpeed :: Int
    , players :: [(String, TMVar Int, [ProgressPointDto])]
    }

data PlayerEnv = PlayerEnv
    { putPoint :: ProgressPointDto -> IO () --queue :: TQueue (String, ProgressPointDto)
    , timeVar :: TMVar Int
    , playerId :: String
    , progressPts :: [ProgressPointDto]
    }    

player :: ReaderT PlayerEnv IO ()
player = do
    allPts <- asks progressPts
    tmv <- asks timeVar
    currTime <- atomically $ takeTMVar tmv
    update currTime allPts >>= run currTime

    where 
        update :: Int -> [ProgressPointDto] -> ReaderT PlayerEnv IO [ProgressPointDto]
        update currTime currPts =
            case currPts of
                p : ps | p.time > currTime -> do
                    f <- asks putPoint
                    --atomically $ writeTQueue env.queue (env.playerId, p)
                    liftIO $ f p
                    update currTime ps
                _ ->
                    pure currPts 

        run :: Int -> [ProgressPointDto] -> ReaderT PlayerEnv IO ()
        run lastTime pts = do
            tmv <- asks timeVar
            currTime <- atomically $ takeTMVar tmv

            if currTime > lastTime
            then 
                update currTime pts >>= run currTime 
            else 
                asks progressPts >>= update currTime >>= run currTime
                
timer :: ReaderT DemoEnv IO ()
timer = do
    -- env <- ask
    -- liftIO $ 
    --     mapConcurrently_ 
    --         (runReaderT player)
    --         (initPlayerEnvs env)
    liftIO $ print "Starting timer"
    asks start >>= loop
    where
        putTime :: Int -> TMVar Int -> IO ()
        putTime t tmv =
            atomically $ putTMVar tmv t

        updateTime :: Int -> ReaderT DemoEnv IO ()
        updateTime t = do
            timeVars <- asks (fmap (\(_, tmv, _) -> tmv) . players)
            liftIO $ 
                mapConcurrently_ (putTime t) timeVars

        loop :: Int -> ReaderT DemoEnv IO ()
        loop currTime = do
            endTime <- asks end
            pSpeed <- asks playSpeed
            -- print currTime
            if currTime > endTime
            then do
                asks start >>= loop 
            else do
                updateTime currTime
                liftIO $ threadDelay 100000
                loop (currTime + 100 * pSpeed)

-- play :: TQueue (String, ProgressPointDto) -> [(String, TaskProgress)] -> Int -> Int -> IO ()
-- play queue items startTime endTime = do
--     loop startTime
--     where
--         loop :: Int -> IO ()
--         loop time =
--             if time >= endTime 
--             then 
--                 loop startTime
--             else do
--                 mapConcurrently_ 
--                     (\(id, tp) -> goOne id (toDto <$> tp.points) time) 
--                     items
--                 threadDelay 100000
--                 loop (time + 100)

--         goOne :: String -> [ProgressPointDto] -> Int -> IO ()
--         goOne id pts time =
--             case pts of
--                 p : rest | time >= p.time -> do
--                     atomically $ writeTQueue queue (id, p)
--                     goOne id rest time
--                 _ -> 
--                     pass

runPlayback :: TMVar (String, ProgressPointDto) -> Int -> IO ()
runPlayback var speed = do
    print "Loading demo tracks..."
    
    demoEnv <- runExceptT initDemoEnv

    case demoEnv of
        Right env -> do
            concurrently_ 
                (mapConcurrently_ (runReaderT player) (initPlayerEnvs env))
                (runReaderT timer env)

            print "Finished playback"
                
        Left err -> 
            print $ "Failed to initialize demo: " <> err

    where 
        initPlayers :: Entity Int32 FlightTask -> [(TrackFileInfo, FlightTrack)] -> IO [(String, TMVar Int, [ProgressPointDto])]
        initPlayers ft =
           mapConcurrently (toPlayerItem ft) 

        toPlayerItem :: Entity Int32 FlightTask -> (TrackFileInfo, FlightTrack) -> IO (String, TMVar Int, [ProgressPointDto])
        toPlayerItem ft (info, track) = do
            tmv <- newEmptyTMVarIO
            pure (info.callsign, tmv, toDto <$> points (progress ft track))

        initDemoEnv :: ExceptT String IO DemoEnv
        initDemoEnv = do  
            files <- liftIO $ findTrackFiles "./demo"
            tracks <- withExceptT (mappend "Failed to load tracks: ")
                        $ traverse (\(i, n) -> (i,) 
                        <$> loadFlightTrack ("./demo/" <> n)) files

            ft <- loadDemoTask

            playersData <- liftIO $ mapConcurrently (toPlayerItem ft) tracks 

            pure $
                DemoEnv 
                    { var = var
                    , start = startTime $ snd <$> tracks
                    , end = endTime $ snd <$> tracks
                    , playSpeed = speed
                    , players = playersData
                    }

        initPlayerEnvs :: DemoEnv -> [PlayerEnv] 
        initPlayerEnvs demoEnv =
            let 
                toPlayerEnv (key, tvar, ppts) =
                    PlayerEnv 
                        { putPoint = \p -> atomically $ tryTakeTMVar demoEnv.var >> putTMVar (demoEnv.var) (key, p)
                        , timeVar = tvar
                        , playerId = key
                        , progressPts = ppts
                        }
            in 
            toPlayerEnv <$> demoEnv.players

runTest :: TMVar (String, ProgressPointDto) -> IO ()
runTest var = do
    -- queue <- atomically (newTQueue :: STM (TQueue Int))
    print "waiting..."
    threadDelay 20000000
    print "going..."
    v <- atomically $ readTMVar var
    -- chan <- atomically $ dupTMChan getChan
    sub var (-1)
    
    -- concurrently_ (push chan 10) (mapConcurrently_ (\id -> atomically (dupTChan chan) >>= sub id) [1..5])
    
    where
        sub :: TMVar (String, ProgressPointDto) -> Int -> IO ()
        sub var lastTime = do
            (id, p) <- atomically $ readTMVar var
            if p.time == lastTime
            then do
                sub var lastTime
            else do
                print (id, p)
                sub var p.time
        -- sub ::  Int -> TChan Int ->IO ()
        -- sub id chan = do
        --     v <- atomically $ readTChan chan
        --     print $ "Channel " <> show id <> ": " <> show v
        --     sub id chan

        -- push :: TChan Int -> Int -> IO ()
        -- push chan v = do
        --     if v <= 0
        --     then 
        --         pass
        --     else do
        --         atomically $ writeTChan chan v
        --         threadDelay 1000000
        --         push chan (v - 1)