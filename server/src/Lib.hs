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
module Lib
    ( startApp
    , app
    ) where

import Relude
import Data.Aeson ( defaultOptions, ToJSON, FromJSON )
import Data.Aeson.TH ( deriveJSON )
import Network.Wai ( Application, Middleware )

import Network.Wai.Handler.Warp ( run )
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
import Session (getNavPointsSession, saveNavPointsSession, deleteDuplicateNavPointsSession, saveFlightTaskSession, getFlightTasksSession)
import Data.Vector (Vector)
import Control.Monad.Except (withExceptT, catchError, mapExcept, mapExceptT)
import Data.Text.Lazy (pack)
import qualified Data.ByteString.Lazy as LBS
import Network.Wai.Middleware.Cors (simpleCors, corsMethods, corsRequestHeaders, corsIgnoreFailures, simpleCorsResourcePolicy, cors)
import Network.HTTP.Types.Method (methodDelete, methodGet, methodOptions, methodPost, methodPut)
import Servant.Multipart (MultipartForm, Mem, MultipartData (files), FileData (fdFileName, fdFileCType, fdPayload), FromMultipart (fromMultipart))
import Data.ByteString (unpack)
import Text.Parsec (parse, ParseError)
import Control.Arrow (ArrowChoice(left))
import Control.Monad.Error.Class (liftEither)
import Statement (saveNavPointsStatement)
import FlightTask (FlightTask)
import Entity (Entity)


data TurnpointType
    = Cylinder
    | Quadrant
    | Line
    deriving (Eq, Show, Read)

data FixValidity
    = Gps3D
    | Baro2D
    deriving (Eq, Show, Read)

data LibError
    = ConnectionError Connection.ConnectionError
    | QueryError Session.QueryError
    deriving (Show)

getConn :: ExceptT LibError IO Connection.Connection
getConn
    = withExceptT ConnectionError
    $ ExceptT
    $ Connection.acquire
    $ Connection.settings "localhost" 5433 "admin" "admin" "cvdb"


getNavPoints :: ExceptT LibError IO (Vector NavPoint)
getNavPoints = 
    getConn >>= withExceptT QueryError . ExceptT . Session.run getNavPointsSession

saveNavPoints :: Vector NavPoint -> ExceptT LibError IO (Int64, Int64)
saveNavPoints nps = do
    conn <- getConn 
    deleted <- withExceptT QueryError . ExceptT $ Session.run (deleteDuplicateNavPointsSession $ toText . name <$> nps) conn
    added <- withExceptT QueryError . ExceptT $ Session.run (saveNavPointsSession nps) conn
    pure (deleted, added)

getFlightTasks :: ExceptT LibError IO [Entity Int32 FlightTask]
getFlightTasks = do
    conn <- getConn
    withExceptT QueryError . ExceptT $ Session.run getFlightTasksSession conn


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

instance FromMultipart Mem [NavPoint] where
    fromMultipart form =
        let
            fls = left show . parseLines . decodeUtf8 . fdPayload <$> files form
            parseLines = parse navPointLinesParser "Multipoart form data"
        in
            concat <$> sequence fls

upload :: [NavPoint] -> Handler (Int, Int64, Int64)
upload navPoints = do
    result <- liftIO $ runExceptT $ saveNavPoints $ fromList navPoints
    case result of
        Left e -> throwError $ err400 { errBody = "Error: " <> (encodeUtf8 . pack . show) e  }
        Right (d, a) -> pure (length navPoints, d, a)

flightTasks :: Handler [Entity Int32 FlightTask]
flightTasks = do
    result <- liftIO $ runExceptT getFlightTasks
    case result of
        Left e -> throwError $ err400 { errBody = "Error: " <> (encodeUtf8 . pack . show) e  }
        Right v -> pure v

saveTask :: FlightTask -> Handler Int64
saveTask flightTask = do
    result <- liftIO $ runExceptT $ saveFlightTask flightTask
    case result of
        Left e -> throwError $ err400 { errBody = "Error: " <> (encodeUtf8 . pack . show) e  }
        Right x -> pure x



type API =
    "upload" :> MultipartForm Mem [NavPoint] :> Post '[JSON] (Int, Int64, Int64)
    :<|> "navpoints" :> Get '[JSON] (Vector NavPoint)
    :<|> "task" :> Get '[JSON] [Entity Int32 FlightTask]
    :<|> "task" :> ReqBody '[JSON] FlightTask :> Post '[JSON] Int64

startApp :: IO ()
startApp = run 8081 app

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
    upload
    :<|> navPoints
    :<|> flightTasks
    :<|> saveTask

