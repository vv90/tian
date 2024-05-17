{-# OPTIONS_GHC -Wno-orphans #-}

module Lib (startApp, app) where

import Aprs.AprsMessage (DeviceId (..))
import Aprs.LocationDatapoint (LocationDatapoint)
import Aprs.Utils (AprsMessageBroker, FlightsState)
import Backend.FlightsState (FlightInformation, FlightPosition)
import Conduit (ConduitT, ResourceT, bracketP, yield)
import Control.Arrow (ArrowChoice (left))
import Control.Concurrent.STM (modifyTVar)
import Control.Concurrent.STM.TBChan (TBChan, newTBChanIO, readTBChan)
import Control.Monad.Except (withExceptT)
import Data.Aeson as Aeson
import Data.ByteString qualified as BS
import Data.HashMap.Strict qualified as HM
import Data.List.NonEmpty qualified as NE (cons, reverse)
import Data.Time (UTCTime (..), getCurrentTime)
import Data.Time.Calendar (fromGregorian)
import Data.UUID (UUID)
import Data.UUID.V4 (nextRandom)
import Data.Vector (Vector)
import Demo.DemoTask (loadDemoTask)
import Demo.NameMatch (NameMatch, loadNames)
import Entity (Entity (..))
import FlightTask (FlightTask)
import FlightTrack (FlightTrack (..))
import FlightTrack.Parser (buildFlightTrack, flightInfoParserAll)
import Geo (Elevation (..), Latitude, Longitude)
import GlidingUtils (TotalEnergyPoint, calculateTotalEnergy)
import Hasql.Session qualified as Session
import NavPoint (NavPoint, name, navPointLinesParser)
import Network.Wai.Handler.Warp (Port, run)
import Network.Wai.Middleware.Gzip
import Persistence.Connection (getConnection)
import Persistence.Session (deleteDuplicateNavPointsSession, getAllFlightTasksSession, getFlightTaskSession, getNavPointsSession, saveFlightTaskSession, saveNavPointsSession)
import Relude
import Servant
import Servant.API.WebSocketConduit (WebSocketConduit)
import Servant.Multipart (FileData (fdFileName, fdPayload), FromMultipart (fromMultipart), Mem, MultipartData (files), MultipartForm)
import System.Directory (listDirectory)
import System.FilePath (takeFileName, (</>))
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
    :<|> "demoTask" :> Get '[JSON] (FlightTask, [NameMatch])
    :<|> "watchFlights" :> WebSocketConduit () (DeviceId, FlightPosition)
    :<|> "flightInfo" :> Capture "deviceId" Text :> Get '[JSON] (Maybe FlightInformation)
    :<|> "currentFlights" :> Get '[JSON] [(DeviceId, (FlightInformation, FlightPosition))]
    :<|> "totalEnergy" :> Capture "deviceId" Text :> Get '[JSON] (Maybe [TotalEnergyPoint])

server :: TVar AprsMessageBroker -> TVar FlightsState -> Server API
server broker flights =
  uploadNavPoints
    :<|> navPoints
    :<|> flightTasks
    :<|> saveTask
    :<|> uploadFlightTrack
    :<|> testTaskProgress
    :<|> testStartLine
    :<|> demoTask
    :<|> watchFlights broker
    :<|> lookupFlightInformation flights
    :<|> currentFlights flights
    :<|> totalEnergy

startApp :: TVar AprsMessageBroker -> TVar FlightsState -> Port -> IO ()
startApp broker flights port = do
  putStrLn ("Server started on port " <> show port)
  run port (app broker flights)

app :: TVar AprsMessageBroker -> TVar FlightsState -> Application
app broker flights =
  server broker flights
    & serve api
    & gzip def

api :: Proxy API
api = Proxy

watchFlights :: TVar AprsMessageBroker -> ConduitT () (DeviceId, FlightPosition) (ResourceT IO) ()
watchFlights broker =
  let makeChan :: IO (UUID, TBChan (DeviceId, FlightPosition))
      makeChan = do
        chanId <- nextRandom
        chan <- newTBChanIO 1000
        atomically $ modifyTVar broker (HM.insert chanId chan)
        pure (chanId, chan)

      disposeChan :: (UUID, TBChan (DeviceId, FlightPosition)) -> IO ()
      disposeChan (chanId, _) =
        atomically $ modifyTVar broker (HM.delete chanId)

      readChan :: TBChan (DeviceId, FlightPosition) -> ConduitT () (DeviceId, FlightPosition) (ResourceT IO) ()
      readChan chan = do
        msg <- atomically $ readTBChan chan
        yield msg
        readChan chan
   in bracketP
        makeChan
        disposeChan
        (readChan . snd)

demoTask :: Handler (FlightTask, [NameMatch])
demoTask = do
  flightTask <- liftIO $ runExceptT loadDemoTask
  nameMatch <- liftIO $ runExceptT loadNames

  case (,) <$> flightTask <*> nameMatch of
    Left e -> throwError $ err400 {errBody = "Error: " <> (encodeUtf8 . toLText) e}
    Right (Entity _ ft, nm) -> pure (ft, nm)

lookupFlightInformation :: TVar FlightsState -> Text -> Handler (Maybe FlightInformation)
lookupFlightInformation flights deviceId = do
  val <- atomically $ HM.lookup (DeviceId deviceId) <$> readTVar flights
  pure $ fmap fst val

currentFlights :: TVar FlightsState -> Handler [(DeviceId, (FlightInformation, FlightPosition))]
currentFlights flights =
  liftIO $ atomically $ HM.toList <$> readTVar flights

totalEnergy :: Text -> Handler (Maybe [TotalEnergyPoint])
totalEnergy deviceId =
  let logsDirectory :: FilePath
      logsDirectory = "logs"

      findDeviceFile :: FilePath -> IO (Maybe FilePath)
      findDeviceFile dirPath =
        find (\path -> toString deviceId `isPrefixOf` takeFileName path) <$> listDirectory dirPath

      readDeviceFile :: FilePath -> Handler [TotalEnergyPoint]
      readDeviceFile fp = do
        content <- liftIO $ BS.split 10 <$> readFileBS fp
        locationPoints <- either (\e -> throwError $ err500 {errBody = "Error: " <> (encodeUtf8 . toLText) e}) pure $ traverse (Aeson.eitherDecodeStrict @LocationDatapoint) (filter (\bs -> BS.length bs > 0) content)
        pure $ calculateTotalEnergy <$> locationPoints
   in do
        todaysDirectoryName <- show . utctDay <$> liftIO getCurrentTime
        maybeFile <- liftIO $ findDeviceFile (logsDirectory </> todaysDirectoryName)
        traverse (\fp -> readDeviceFile (logsDirectory </> todaysDirectoryName </> fp)) maybeFile
