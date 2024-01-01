{-# OPTIONS_GHC -Wno-orphans #-}

module Lib (startApp, app) where

import Aprs.Utils (FlightId, FlightPosition, FlightsTable)
import Conduit (ConduitT, ResourceT, yield)
import Control.Arrow (ArrowChoice (left))
import Control.Concurrent.STM.TBChan (readTBChan)
import Control.Monad.Except (withExceptT)
import Data.HashMap.Strict qualified as HM
import Data.List.NonEmpty qualified as NE (cons, reverse)
import Data.Time (UTCTime (UTCTime))
import Data.Time.Calendar (fromGregorian)
import Data.Vector (Vector)
import Demo.DemoConduit (demoC)
import Demo.DemoTask (loadDemoTask)
import Demo.NameMatch (NameMatch, loadNames)
import Entity (Entity (..))
import FlightTask (FlightTask)
import FlightTrack (FlightTrack (..))
import FlightTrack.Parser (buildFlightTrack, flightInfoParserAll)
import Geo (Elevation (..), Latitude, Longitude)
import Hasql.Session qualified as Session
import NavPoint (NavPoint, name, navPointLinesParser)
import Network.Wai.Handler.Warp (Port, run)
import Network.Wai.Middleware.Gzip
import Persistence.Connection (getConnection)
import Persistence.Session (deleteDuplicateNavPointsSession, getAllFlightTasksSession, getFlightTaskSession, getNavPointsSession, saveFlightTaskSession, saveNavPointsSession)
import ProgressPoint (ProgressPointDto)
import Relude
import Servant
import Servant.API.WebSocketConduit (WebSocketConduit, WebSocketSource)
import Servant.Multipart (FileData (fdFileName, fdPayload), FromMultipart (fromMultipart), Mem, MultipartData (files), MultipartForm)
import TaskProgress (TaskProgressDto, toDto)
import TaskProgressUtils (progress, taskStartLine)
import Text.Parsec (Parsec, parse)
import TrackPoint (FixValidity (..), TrackPoint (..))

data LibError
  = ConnectionError Text
  | QueryError Session.QueryError
  | FormatError Text
  | EnvironmentError Text
  deriving stock (Show)

instance ToLText LibError where
  toLText :: LibError -> LText
  toLText = show

-- catchLiftIO :: IO a -> ExceptT LibError IO a
-- catchLiftIO = withExceptT EnvironmentError . ExceptT . try

-- getConn :: ExceptT LibError IO Connection.Connection
-- getConn =
--   let tryGetEnv =
--         withExceptT (EnvironmentError . show)
--           . ExceptT
--           . (try :: IO a -> IO (Either SomeException a))
--           . getEnv
--       -- host = catchLiftIO $ getEnv "DB_HOST"
--       tryGetPort str = do
--         pstr <- tryGetEnv str
--         withExceptT EnvironmentError $ ExceptT $ (pure :: a -> IO a) $ (readEither :: String -> Either Text Word16) pstr

--       conn host port usr pwd db =
--         withExceptT ConnectionError
--           $ ExceptT
--           $ Connection.acquire
--           $ Connection.settings host port usr pwd db
--    in do
--         host <- tryGetEnv "DB_HOST"
--         port <- tryGetPort "DB_PORT"
--         usr <- tryGetEnv "DB_USER"
--         pwd <- tryGetEnv "DB_PASS"
--         db <- tryGetEnv "DB_NAME"

--         conn (encodeUtf8 host) port (encodeUtf8 usr) (encodeUtf8 pwd) (encodeUtf8 db)

-- dbhost <- catchLiftIO $ getEnv "DB_HOST"
-- dbport <- catchLiftIO $ getEnv "DB_PORT"
-- dbuser <- catchLiftIO $ getEnv "DB_USER"
-- dbpass <- catchLiftIO $ getEnv "DB_PASS"
-- dbname <- catchLiftIO $ getEnv "DB_NAME"

-- withExceptT ConnectionError
-- \$ ExceptT
-- \$ Connection.acquire
-- \$ Connection.settings "localhost" 5433 "admin" "admin" "cvdb"

-- detectAndDecode :: ByteString -> Either String String
-- detectAndDecode bs =
--     case detectEncodingName (toStrict bs) of
--         Just "UTF-8" -> left show $ decodeUtf8Strict bs
--         Just unsupported -> Left $ "Unsupported encoding: " <> unsupported
--         Nothing -> Left "Failed to detect encoding"

getNavPoints :: ExceptT LibError IO (Vector NavPoint)
getNavPoints =
  withExceptT ConnectionError getConnection >>= withExceptT QueryError . ExceptT . Session.run getNavPointsSession

saveNavPoints :: Vector NavPoint -> ExceptT LibError IO (Int64, Int64)
saveNavPoints nps = do
  conn <- withExceptT ConnectionError getConnection
  deleted <- withExceptT QueryError . ExceptT $ Session.run (deleteDuplicateNavPointsSession $ toText . name <$> nps) conn
  added <- withExceptT QueryError . ExceptT $ Session.run (saveNavPointsSession nps) conn
  pure (deleted, added)

getAllFlightTasks :: ExceptT LibError IO [Entity Int32 FlightTask]
getAllFlightTasks = do
  conn <- withExceptT ConnectionError getConnection
  withExceptT QueryError . ExceptT $ Session.run getAllFlightTasksSession conn

getFlightTask :: Int32 -> ExceptT LibError IO (Maybe (Entity Int32 FlightTask))
getFlightTask taskId = do
  conn <- withExceptT ConnectionError getConnection
  withExceptT QueryError . ExceptT $ Session.run (getFlightTaskSession taskId) conn

saveFlightTask :: FlightTask -> ExceptT LibError IO Int64
saveFlightTask ft = do
  conn <- withExceptT ConnectionError getConnection
  savedTpsNum <- withExceptT QueryError . ExceptT $ Session.run (saveFlightTaskSession ft) conn
  pure $ savedTpsNum + 1

navPoints :: Handler (Vector NavPoint)
navPoints = do
  result <- liftIO $ runExceptT getNavPoints
  case result of
    Left e -> throwError $ err400 {errBody = "Error: " <> (encodeUtf8 . toLText) e}
    Right v -> pure v

parseFile :: forall a. Parsec Text () a -> FileData Mem -> Either String a
parseFile parser fileData =
  let fileName = toString $ fdFileName fileData
      parseContent :: Text -> Either String a
      parseContent = left show . parse parser fileName
   in parseContent . decodeUtf8 . fdPayload $ fileData

instance FromMultipart Mem [NavPoint] where
  fromMultipart :: MultipartData Mem -> Either String [NavPoint]
  fromMultipart form =
    let navPointResults = parseFile navPointLinesParser <$> files form
     in concat <$> sequence navPointResults

instance FromMultipart Mem [FlightTrack] where
  fromMultipart form =
    let flightTrackResults = (\x -> parseFile flightInfoParserAll x >>= buildFlightTrack (show $ fdFileName x)) <$> files form
     in sequence flightTrackResults

uploadNavPoints :: [NavPoint] -> Handler (Int, Int64, Int64)
uploadNavPoints pts = do
  result <- liftIO $ runExceptT $ saveNavPoints $ fromList pts
  case result of
    Left e -> throwError $ err400 {errBody = "Error: " <> (encodeUtf8 . toLText) e}
    Right (d, a) -> pure (length pts, d, a)

uploadFlightTrack :: Int32 -> [FlightTrack] -> Handler [TaskProgressDto]
uploadFlightTrack taskId tracks = do
  flightTask <- liftIO $ runExceptT $ getFlightTask taskId

  case flightTask of
    Left e -> throwError $ err400 {errBody = "Error: " <> (encodeUtf8 . toLText) e}
    Right Nothing -> throwError $ err400 {errBody = "Error: " <> encodeUtf8 ("Task not found" :: LText)}
    Right (Just ft) -> do
      pure $ toDto . progress ft <$> tracks

testStartLine :: Int32 -> Handler ((Latitude, Longitude), (Latitude, Longitude))
testStartLine taskId = do
  flightTask <-
    fmap (\a -> a >>= maybeToRight (FormatError "Task not found"))
      $ liftIO
      $ runExceptT
      $ getFlightTask taskId
  case flightTask of
    Left e -> throwError $ err400 {errBody = "Error: " <> (encodeUtf8 . toLText) e}
    Right (Entity _ ft) -> pure $ taskStartLine ft

flightTasks :: Handler [Entity Int32 FlightTask]
flightTasks = do
  result <- liftIO $ runExceptT getAllFlightTasks
  case result of
    Left e -> throwError $ err400 {errBody = "Error: " <> (encodeUtf8 . toLText) e}
    Right v -> pure v

saveTask :: FlightTask -> Handler Int64
saveTask flightTask = do
  result <- liftIO $ runExceptT $ saveFlightTask flightTask
  case result of
    Left e -> throwError $ err400 {errBody = "Error: " <> (encodeUtf8 . toLText) e}
    Right x -> pure x

testTaskProgress :: Int32 -> NonEmpty (Latitude, Longitude) -> Handler TaskProgressDto
testTaskProgress taskId (pt :| points) =
  let toTrackPoint t (lat, lon) =
        TrackPoint
          { time = t,
            lat = lat,
            lon = lon,
            fixValidity = Gps3D,
            altitudeBaro = ElevationMeters 1000,
            altitudeGps = ElevationMeters 1000
          }

      _date = UTCTime (fromGregorian 2020 1 1) 0
      pts =
        fst
          $ foldl'
            (\(b, i) a -> (NE.cons (toTrackPoint (i + 1) a) b, i + 1))
            (toTrackPoint 1 pt :| [], 1)
            points
      track =
        FlightTrack
          { date = UTCTime (fromGregorian 2020 1 1) 0,
            compId = "test",
            points = NE.reverse pts
          }

      _unwrapTaskMaybe =
        maybeToRight (FormatError "Task not found")
   in do
        flightTask <-
          fmap (\a -> a >>= maybeToRight (FormatError "Task not found"))
            $ liftIO
            $ runExceptT
            $ getFlightTask taskId

        let res = progress <$> flightTask <*> pure track

        case res of
          Left e -> throwError $ err400 {errBody = "Error: " <> (encodeUtf8 . toLText) e}
          Right x -> pure $ toDto x

type API =
  "navpoints" :> MultipartForm Mem [NavPoint] :> Post '[JSON] (Int, Int64, Int64)
    :<|> "navpoints" :> Get '[JSON] (Vector NavPoint)
    :<|> "task" :> Get '[JSON] [Entity Int32 FlightTask]
    :<|> "task" :> ReqBody '[JSON] FlightTask :> Post '[JSON] Int64
    :<|> "track" :> Capture "taskId" Int32 :> MultipartForm Mem [FlightTrack] :> Post '[JSON] [TaskProgressDto]
    :<|> "test" :> "taskProgress" :> Capture "taskId" Int32 :> ReqBody '[JSON] (NonEmpty (Latitude, Longitude)) :> Post '[JSON] TaskProgressDto
    :<|> "test" :> "startLine" :> Capture "taskId" Int32 :> Get '[JSON] ((Latitude, Longitude), (Latitude, Longitude))
    :<|> "demo" :> WebSocketSource (Text, ProgressPointDto)
    :<|> "demoTask" :> Get '[JSON] (FlightTask, [NameMatch])
    :<|> "watchFlights" :> WebSocketConduit () (FlightId, FlightPosition)
    :<|> "elevationTile" :> Capture "zoom" Int :> Capture "x" Int :> Capture "y" Int :> Get '[OctetStream] ByteString

server :: TVar FlightsTable -> Server API
server flightsTvar =
  uploadNavPoints
    :<|> navPoints
    :<|> flightTasks
    :<|> saveTask
    :<|> uploadFlightTrack
    :<|> testTaskProgress
    :<|> testStartLine
    :<|> progressDemo
    :<|> demoTask
    :<|> watchFlights flightsTvar
    :<|> elevationTile

startApp :: TVar FlightsTable -> Port -> IO ()
startApp flightsTvar port = do
  putStrLn ("Server started on port " <> show port)
  run port (app flightsTvar)

app :: TVar FlightsTable -> Application
app flightsTvar =
  server flightsTvar
    & serve api
    & gzip def

api :: Proxy API
api = Proxy

watchFlights :: TVar FlightsTable -> ConduitT () (FlightId, FlightPosition) (ResourceT IO) ()
watchFlights flightsTvar =
  let readAllChans :: ConduitT () (FlightId, FlightPosition) (ResourceT IO) ()
      readAllChans = do
        chans <- HM.toList <$> readTVarIO flightsTvar
        traverse_ (\(flightId, chan) -> atomically (readTBChan chan) >>= (yield . (flightId,))) chans
        readAllChans
   in readAllChans

progressDemo :: ConduitT () (Text, ProgressPointDto) (ResourceT IO) ()
progressDemo = do
  ft <- liftIO $ runExceptT loadDemoTask

  case ft of
    Right (Entity _ x) -> do
      -- chan <- liftIO getChan
      putStrLn "Init demo..."
      demoC x
    -- resultC chan

    -- sourceTMChan chan
    -- liftIO $ withAsync (runDemo q x) $ \r -> do
    -- liftIO (runDemo x) >> demo x
    -- print "Connecting socket..."

    -- sourceTQueue q
    -- sourceTChan
    -- sourceQueue queue

    Left e ->
      liftIO $ print e

-- where
--     resultC :: TMChan (String, ProgressPointDto) -> ConduitT () (String, ProgressPointDto) (ResourceT IO) ()
--     resultC chan = do
--         r <- liftIO $ atomically (readTMChan chan)
--         case r of
--             Just x -> do
--                 liftIO $ print "sending..."
--                 -- yield x
--                 resultC chan
--             Nothing -> do
--                 liftIO $ print "waiting..."
--                 resultC chan

demoTask :: Handler (FlightTask, [NameMatch])
demoTask = do
  flightTask <- liftIO $ runExceptT loadDemoTask
  nameMatch <- liftIO $ runExceptT loadNames

  case (,) <$> flightTask <*> nameMatch of
    Left e -> throwError $ err400 {errBody = "Error: " <> (encodeUtf8 . toLText) e}
    Right (Entity _ ft, nm) -> pure (ft, nm)

elevationTile :: Int -> Int -> Int -> Handler ByteString
elevationTile zoom x y = do
  readFileBS $ "./tiles/" <> show zoom <> "/" <> show x <> "_" <> show y <> ".json"

-- case pts of
--     Left e -> throwError $ err400 { errBody = "Error: " <> (encodeUtf8 . pack . show) e  }
--     Right x -> pure x

-- startDemo :: TMVar FlightTask -> Handler ()
-- startDemo var = do
--     print "attempting to start demo"
--     flightTask <-
--         fmap (\a -> a >>= maybeToRight (FormatError "Task not found"))
--         $ liftIO $ runExceptT $ getFlightTask 6

--     case flightTask of
--         Left e ->
--             throwError $ err400 { errBody = "Error: " <> (encodeUtf8 . pack . show) e  }
--         Right (Entity _ ft) ->
--             liftIO $ atomically $ putTMVar var ft
