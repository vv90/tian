{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE TypeOperators              #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE UndecidableInstances       #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE DeriveAnyClass             #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE InstanceSigs #-}
module Lib
    ( startApp
    , app
    ) where

import Relude
import Data.Aeson ( defaultOptions, ToJSON, FromJSON )
import Data.Aeson.TH ( deriveJSON )
import Network.Wai ( Application, Middleware )

import Network.Wai.Handler.Warp ( run, Port )
import Servant
-- import Servant.Multipart
import Control.Monad.IO.Class
    ( MonadIO(liftIO) )
import Control.Monad.Reader
import Control.Monad.Writer
import Data.Time (UTCTime(UTCTime))
import Control.Monad.Logger (NoLoggingT)
import Data.Char (toUpper)
import GHC.Generics (Generic)
import NavPoint (NavPoint, navPointLinesParser, name)
import qualified Hasql.Connection as Connection
import qualified Hasql.Session as Session
import Session (getNavPointsSession, saveNavPointsSession, deleteDuplicateNavPointsSession, saveFlightTaskSession, getAllFlightTasksSession, getFlightTaskSession)
import Data.Vector (Vector)
import Control.Monad.Except (withExceptT, catchError, mapExcept, mapExceptT)
import Data.Text.Lazy (pack)
import qualified Data.ByteString.Lazy as LBS
import Network.Wai.Middleware.Cors (simpleCors, corsMethods, corsRequestHeaders, corsIgnoreFailures, simpleCorsResourcePolicy, cors)
import Network.HTTP.Types.Method (methodDelete, methodGet, methodOptions, methodPost, methodPut)
import Servant.Multipart (MultipartForm, Mem, MultipartData (files), FileData (fdFileName, fdFileCType, fdPayload), FromMultipart (fromMultipart))
import Data.ByteString (unpack)
import Text.Parsec (parse, ParseError, Parsec)
import Control.Arrow (ArrowChoice(left))
import Control.Monad.Error.Class (liftEither)
import Statement (saveNavPointsStatement)
import FlightTask (FlightTask)
import Entity (Entity (..))
import FlightTrack (FlightInfo, flightInfoParser, buildFlightTrack, FlightTrack (..), date, compId, points)
import TaskProgressUtils (progress, progressInit, progressAdvance, taskStartLine)
import ProgressPoint (ProgressPoint)
import TaskProgress (TaskProgress, TaskProgressDto)
import qualified TaskProgress
import Debug.Trace as Debug
import TaskProgress (toDto)
import Geo.Utils (perpendicular, s84position)
import Geo (Longitude, Latitude, Elevation (ElevationMeters))
import Data.Sequence (mapWithIndex)
import qualified Data.List.NonEmpty as NE (cons, reverse)
import Data.Time.Calendar (fromGregorian)
import TrackPoint (TrackPoint(..), FixValidity (..))
import Servant.API.WebSocketConduit (WebSocketSource)
import Control.Concurrent (yield)

data LibError
    = ConnectionError Connection.ConnectionError
    | QueryError Session.QueryError
    | FormatError Text
    deriving (Show)

getConn :: ExceptT LibError IO Connection.Connection
getConn
    = withExceptT ConnectionError
    $ ExceptT
    $ Connection.acquire
    $ Connection.settings "localhost" 5433 "admin" "admin" "cvdb"

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
        Left e -> throwError $ err400 { errBody = "Error: " <> (encodeUtf8 . pack . show) e  }
        Right v -> pure v

parseFile :: Parsec Text () a -> FileData Mem -> Either String a
parseFile parser fileData =
    let
        fileName = fdFileName fileData
        parseContent = left show . parse parser (toString fileName)
    in
        parseContent . decodeUtf8 . fdPayload $ fileData

instance FromMultipart Mem [NavPoint] where
    fromMultipart :: MultipartData Mem -> Either String [NavPoint]
    fromMultipart form =
        let
            navPointResults = parseFile navPointLinesParser <$> files form
        in
            concat <$> sequence navPointResults

instance FromMultipart Mem [FlightTrack] where
    fromMultipart form =
        let
            flightTrackResults = (parseFile flightInfoParser >=> buildFlightTrack) <$> files form
        in
            sequence flightTrackResults


uploadNavPoints :: [NavPoint] -> Handler (Int, Int64, Int64)
uploadNavPoints navPoints = do
    result <- liftIO $ runExceptT $ saveNavPoints $ fromList navPoints
    case result of
        Left e -> throwError $ err400 { errBody = "Error: " <> (encodeUtf8 . pack . show) e  }
        Right (d, a) -> pure (length navPoints, d, a)

uploadFlightTrack :: Int32 -> [FlightTrack] -> Handler [TaskProgressDto]
uploadFlightTrack taskId tracks = do
    flightTask <- liftIO $ runExceptT $ getFlightTask taskId

    case flightTask of
        Left e -> throwError $ err400 { errBody = "Error: " <> (encodeUtf8 . pack . show) e  }
        Right Nothing -> throwError $ err400 { errBody = "Error: " <> (encodeUtf8 . pack . show) "Task not found"  }
        Right (Just ft) -> do
            pure $ toDto . progress ft <$> tracks

testStartLine :: Int32 -> Handler ((Latitude, Longitude), (Latitude, Longitude))
testStartLine taskId = do
    flightTask <- 
        fmap (\a -> a >>= maybeToRight (FormatError "Task not found")) 
        $ liftIO $ runExceptT $ getFlightTask taskId
    case flightTask of
        Left e -> throwError $ err400 { errBody = "Error: " <> (encodeUtf8 . pack . show) e  }
        Right (Entity _ ft) -> pure $ taskStartLine ft


flightTasks :: Handler [Entity Int32 FlightTask]
flightTasks = do
    result <- liftIO $ runExceptT getAllFlightTasks
    case result of
        Left e -> throwError $ err400 { errBody = "Error: " <> (encodeUtf8 . pack . show) e  }
        Right v -> pure v

saveTask :: FlightTask -> Handler Int64
saveTask flightTask = do
    result <- liftIO $ runExceptT $ saveFlightTask flightTask
    case result of
        Left e -> throwError $ err400 { errBody = "Error: " <> (encodeUtf8 . pack . show) e  }
        Right x -> pure x

testTaskProgress :: Int32 -> NonEmpty (Latitude, Longitude) -> Handler TaskProgressDto
testTaskProgress taskId (first :| points) =
    let toTrackPoint t (lat, lon) = 
            TrackPoint
                { time = t 
                , lat = lat 
                , lon = lon 
                , fixValidity = Gps3D
                , altitudeBaro = ElevationMeters 1000 
                , altitudeGps = ElevationMeters 1000
                }
      
        date = UTCTime (fromGregorian 2020 1 1) 0
        pts = 
            fst $
            foldl' 
                (\(b, i) a -> (NE.cons (toTrackPoint (i+1) a) b, i+1) ) 
                (toTrackPoint 1 first :| [], 1) 
                points 
        track = 
            FlightTrack
                { date = UTCTime (fromGregorian 2020 1 1) 0
                , compId = "test"
                , points = NE.reverse pts   
                }
        
        unwrapTaskMaybe = 
            maybeToRight (FormatError "Task not found")
    in do
    
    flightTask <- 
        fmap (\a -> a >>= maybeToRight (FormatError "Task not found")) 
        $ liftIO $ runExceptT $ getFlightTask taskId

    let res = progress <$> flightTask <*> pure track

    case res of
        Left e -> throwError $ err400 { errBody = "Error: " <> (encodeUtf8 . pack . show) e  }
        Right x -> pure $ toDto x


type API =
    "navpoints" :> MultipartForm Mem [NavPoint] :> Post '[JSON] (Int, Int64, Int64)
    :<|> "navpoints" :> Get '[JSON] (Vector NavPoint)
    :<|> "task" :> Get '[JSON] [Entity Int32 FlightTask]
    :<|> "task" :> ReqBody '[JSON] FlightTask :> Post '[JSON] Int64
    :<|> "track" :> Capture "taskId" Int32 :> MultipartForm Mem [FlightTrack] :> Post '[JSON] [TaskProgressDto]
    :<|> "test" :> "taskProgress" :> Capture "taskId" Int32 :> ReqBody '[JSON] (NonEmpty (Latitude, Longitude)) :> Post '[JSON] TaskProgressDto
    :<|> "test" :> "startLine" :> Capture "taskId" Int32 :> Get '[JSON] ((Latitude, Longitude), (Latitude, Longitude))
    -- :<|> "ws" :> WebSocketSource Text
        

startApp :: Port -> IO ()
startApp port = do
    putStrLn ("Server started on port " <> show port) 
    run port app

app :: Application
app = corsMiddleware $ serve api server
    where
        corsMiddleware :: Middleware
        corsMiddleware = cors (const $ Just corsPolicy)

        corsPolicy =
            simpleCorsResourcePolicy
                { corsMethods = [methodOptions, methodGet, methodPost, methodPut, methodDelete]
                -- Note: Content-Type header is necessary for POST requests
                , corsRequestHeaders = ["Authorization", "Origin", "Content-Type", "Browser-Locale-Data"]
                -- , corsIgnoreFailures = True
                }

api :: Proxy API
api = Proxy


server :: Server API
server =
    uploadNavPoints
    :<|> navPoints
    :<|> flightTasks
    :<|> saveTask
    :<|> uploadFlightTrack
    :<|> testTaskProgress
    :<|> testStartLine

