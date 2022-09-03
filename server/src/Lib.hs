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
import Session (getNavPointsSession, saveNavPointsSession, deleteDuplicateNavPointsSession)
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



data TurnpointType
    = Cylinder
    | Quadrant
    | Line
    deriving (Eq, Show, Read)

data FixValidity
    = Gps3D
    | Baro2D
    deriving (Eq, Show, Read)


data Entity a = Entity
    { key :: Int
    , val :: a
    } deriving (Show, ToJSON, FromJSON, Generic)


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


type API =
    "upload" :> MultipartForm Mem [NavPoint] :> Post '[JSON] (Int, Int64, Int64)
    :<|> "navpoints" :> Get '[JSON] (Vector NavPoint)

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

