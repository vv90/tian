{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module Lib
  ( startApp,
    app,
  )
where

-- import Servant.Multipart

import Conduit (ConduitT, ResourceT)
import Control.Arrow (ArrowChoice (left))
import Control.Exception (try)
import Control.Monad.Error.Class (liftEither)
import Control.Monad.Except (catchError, mapExcept, mapExceptT, withExceptT)
import Control.Monad.IO.Class
  ( MonadIO (liftIO),
  )
import Control.Monad.Logger (NoLoggingT)
import Data.Aeson (FromJSON, ToJSON, defaultOptions)
import Data.Aeson.TH (deriveJSON)
import Data.ByteString (unpack)
import Data.ByteString.Lazy qualified as LBS
import Data.Char (toUpper)
import Data.List.NonEmpty qualified as NE (cons, reverse)
import Data.Sequence (mapWithIndex)
import Data.Text.Lazy (pack)
import Data.Time (UTCTime (UTCTime))
import Data.Time.Calendar (fromGregorian)
import Data.Vector (Vector)
import Demo.DemoConduit (demoC)
import Demo.DemoTask (loadDemoTask)
import Demo.NameMatch (NameMatch, loadNames)
import Entity (Entity (..))
import FlightTask (FlightTask)
import FlightTrack (FlightTrack (..))
import FlightTrack.Parser (FlightInfo, buildFlightTrack, flightInfoParser, flightInfoParserAll)
import GHC.Generics (Generic)
import Geo (Elevation (..), Latitude, Longitude)
import GeoTiff.Tiff (readElevations, readTiff)
import Hasql.Connection qualified as Connection
import Hasql.Session qualified as Session
import Map (MapTile)
import NavPoint (NavPoint, name, navPointLinesParser)
import Network.HTTP.Types.Method (methodDelete, methodGet, methodOptions, methodPost, methodPut)
import Network.Wai (Application, Middleware)
import Network.Wai.Handler.Warp (Port, run)
import Network.Wai.Middleware.Cors (cors, corsIgnoreFailures, corsMethods, corsRequestHeaders, simpleCors, simpleCorsResourcePolicy)
import ProgressPoint (ProgressPoint, ProgressPointDto)
import Relude
import Servant
import Servant.API.WebSocketConduit (WebSocketSource)
import Servant.Multipart (FileData (fdFileCType, fdFileName, fdPayload), FromMultipart (fromMultipart), Mem, MultipartData (files), MultipartForm)
import Session (deleteDuplicateNavPointsSession, getAllFlightTasksSession, getFlightTaskSession, getNavPointsSession, saveFlightTaskSession, saveNavPointsSession)
import Statement (saveNavPointsStatement)
import System.Environment (getEnv)
import TaskProgress (TaskProgressDto, toDto)
import TaskProgressUtils (progress, progressAdvance, progressInit, taskStartLine)
import Text.Parsec (ParseError, Parsec, parse)
import TrackPoint (FixValidity (..), TrackPoint (..))

data LibError
  = ConnectionError Connection.ConnectionError
  | QueryError Session.QueryError
  | FormatError Text
  | EnvironmentError Text
  deriving (Show)

-- catchLiftIO :: IO a -> ExceptT LibError IO a
-- catchLiftIO = withExceptT EnvironmentError . ExceptT . try

getConn :: ExceptT LibError IO Connection.Connection
getConn =
  let tryGetEnv =
        withExceptT (EnvironmentError . show)
          . ExceptT
          . (try :: IO a -> IO (Either SomeException a))
          . getEnv
      -- host = catchLiftIO $ getEnv "DB_HOST"
      tryGetPort str = do
        pstr <- tryGetEnv str
        withExceptT EnvironmentError $ ExceptT $ (pure :: a -> IO a) $ (readEither :: String -> Either Text Word16) pstr

      conn host port usr pwd db =
        withExceptT ConnectionError
          $ ExceptT
          $ Connection.acquire
          $ Connection.settings host port usr pwd db
   in do
        host <- tryGetEnv "DB_HOST"
        port <- tryGetPort "DB_PORT"
        usr <- tryGetEnv "DB_USER"
        pwd <- tryGetEnv "DB_PASS"
        db <- tryGetEnv "DB_NAME"

        conn (encodeUtf8 host) port (encodeUtf8 usr) (encodeUtf8 pwd) (encodeUtf8 db)

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
  getConn >>= withExceptT QueryError . ExceptT . Session.run getNavPointsSession

saveNavPoints :: Vector NavPoint -> ExceptT LibError IO (Int64, Int64)
saveNavPoints nps = do
  conn <- getConn
  deleted <- withExceptT QueryError . ExceptT $ Session.run (deleteDuplicateNavPointsSession $ toText . name <$> nps) conn
  added <- withExceptT QueryError . ExceptT $ Session.run (saveNavPointsSession nps) conn
  pure (deleted, added)

getAllFlightTasks :: ExceptT LibError IO [Entity Int32 FlightTask]
getAllFlightTasks = do
  conn <- getConn
  withExceptT QueryError . ExceptT $ Session.run getAllFlightTasksSession conn

getFlightTask :: Int32 -> ExceptT LibError IO (Maybe (Entity Int32 FlightTask))
getFlightTask taskId = do
  conn <- getConn
  withExceptT QueryError . ExceptT $ Session.run (getFlightTaskSession taskId) conn

saveFlightTask :: FlightTask -> ExceptT LibError IO Int64
saveFlightTask ft = do
  conn <- getConn
  savedTpsNum <- withExceptT QueryError . ExceptT $ Session.run (saveFlightTaskSession ft) conn
  pure $ savedTpsNum + 1

navPoints :: Handler (Vector NavPoint)
navPoints = do
  result <- liftIO $ runExceptT getNavPoints
  case result of
    Left e -> throwError $ err400 {errBody = "Error: " <> (encodeUtf8 . pack . show) e}
    Right v -> pure v

parseFile :: Parsec Text () a -> FileData Mem -> Either String a
parseFile parser fileData =
  let fileName = fdFileName fileData
      parseContent = left show . parse parser (toString fileName)
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
uploadNavPoints navPoints = do
  result <- liftIO $ runExceptT $ saveNavPoints $ fromList navPoints
  case result of
    Left e -> throwError $ err400 {errBody = "Error: " <> (encodeUtf8 . pack . show) e}
    Right (d, a) -> pure (length navPoints, d, a)

uploadFlightTrack :: Int32 -> [FlightTrack] -> Handler [TaskProgressDto]
uploadFlightTrack taskId tracks = do
  flightTask <- liftIO $ runExceptT $ getFlightTask taskId

  case flightTask of
    Left e -> throwError $ err400 {errBody = "Error: " <> (encodeUtf8 . pack . show) e}
    Right Nothing -> throwError $ err400 {errBody = "Error: " <> (encodeUtf8 . pack . show) "Task not found"}
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
    Left e -> throwError $ err400 {errBody = "Error: " <> (encodeUtf8 . pack . show) e}
    Right (Entity _ ft) -> pure $ taskStartLine ft

flightTasks :: Handler [Entity Int32 FlightTask]
flightTasks = do
  result <- liftIO $ runExceptT getAllFlightTasks
  case result of
    Left e -> throwError $ err400 {errBody = "Error: " <> (encodeUtf8 . pack . show) e}
    Right v -> pure v

saveTask :: FlightTask -> Handler Int64
saveTask flightTask = do
  result <- liftIO $ runExceptT $ saveFlightTask flightTask
  case result of
    Left e -> throwError $ err400 {errBody = "Error: " <> (encodeUtf8 . pack . show) e}
    Right x -> pure x

testTaskProgress :: Int32 -> NonEmpty (Latitude, Longitude) -> Handler TaskProgressDto
testTaskProgress taskId (first :| points) =
  let toTrackPoint t (lat, lon) =
        TrackPoint
          { time = t,
            lat = lat,
            lon = lon,
            fixValidity = Gps3D,
            altitudeBaro = ElevationMeters 1000,
            altitudeGps = ElevationMeters 1000
          }

      date = UTCTime (fromGregorian 2020 1 1) 0
      pts =
        fst
          $ foldl'
            (\(b, i) a -> (NE.cons (toTrackPoint (i + 1) a) b, i + 1))
            (toTrackPoint 1 first :| [], 1)
            points
      track =
        FlightTrack
          { date = UTCTime (fromGregorian 2020 1 1) 0,
            compId = "test",
            points = NE.reverse pts
          }

      unwrapTaskMaybe =
        maybeToRight (FormatError "Task not found")
   in do
        flightTask <-
          fmap (\a -> a >>= maybeToRight (FormatError "Task not found"))
            $ liftIO
            $ runExceptT
            $ getFlightTask taskId

        let res = progress <$> flightTask <*> pure track

        case res of
          Left e -> throwError $ err400 {errBody = "Error: " <> (encodeUtf8 . pack . show) e}
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
    :<|> "elevationPoints" :> ReqBody '[JSON] [MapTile] :> Post '[JSON] [Vector Int]

-- :<|> "startDemo" :> Get '[JSON] ()

startApp :: Port -> IO ()
startApp port = do
  putStrLn ("Server started on port " <> show port)
  run port app

-- app :: Application
-- app = corsMiddleware $ serve api server
--     where
--         corsMiddleware :: Middleware
--         corsMiddleware = cors (const $ Just corsPolicy)

--         corsPolicy =
--             simpleCorsResourcePolicy
--                 { corsMethods = [methodOptions, methodGet, methodPost, methodPut, methodDelete]
--                 -- Note: Content-Type header is necessary for POST requests
--                 , corsRequestHeaders = ["Authorization", "Origin", "Content-Type", "Browser-Locale-Data"]
--                 -- , corsIgnoreFailures = True
--                 }
app :: Application
app = serve api server

api :: Proxy API
api = Proxy

progressDemo :: ConduitT () (Text, ProgressPointDto) (ResourceT IO) ()
progressDemo = do
  ft <- liftIO $ runExceptT loadDemoTask

  case ft of
    Right (Entity _ x) -> do
      -- chan <- liftIO getChan
      print "Init demo..."
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
    Left e -> throwError $ err400 {errBody = "Error: " <> (encodeUtf8 . pack . show) e}
    Right (Entity _ ft, nm) -> pure (ft, nm)

elevationPoints :: [MapTile] -> Handler [Vector Int]
elevationPoints tiles = do
  -- pts <- liftIO $ runExceptT $ readTiff "./demo/ASTGTMV003_N52E039_dem.tif" tiles
  liftIO $ readElevations tiles

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

server :: Server API
server =
  uploadNavPoints
    :<|> navPoints
    :<|> flightTasks
    :<|> saveTask
    :<|> uploadFlightTrack
    :<|> testTaskProgress
    :<|> testStartLine
    :<|> progressDemo
    :<|> demoTask
    :<|> elevationPoints
