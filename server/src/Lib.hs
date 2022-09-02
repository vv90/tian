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
import NavPoint (NavPoint, navPointLinesParser)
import qualified Hasql.Connection as Connection
import qualified Hasql.Session as Session
import Session (getNavPointsSession)
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
-- import Control.Monad.Error (mapErrorT, MonadError (catchError))



data TurnpointType
    = Cylinder
    | Quadrant
    | Line
    deriving (Eq, Show, Read)

data FixValidity
    = Gps3D
    | Baro2D
    deriving (Eq, Show, Read)

data User = User
    { name :: String
    , age :: Int
    } deriving (Show, ToJSON, FromJSON, Generic)

data Entity a = Entity
    { key :: Int
    , val :: a
    } deriving (Show, ToJSON, FromJSON, Generic)


users :: Handler [Entity User]
users = liftIO $ print "Users" >> pure []

postUser :: Int -> Handler ()
postUser _ = liftIO $ print "Post User" >> pure ()

data LibError
    = ConnectionError Connection.ConnectionError
    | QueryError Session.QueryError
    | DataParseError Text
    deriving (Show)

getConn :: ExceptT LibError IO Connection.Connection
getConn
    = withExceptT ConnectionError
    $ ExceptT
    $ Connection.acquire
    $ Connection.settings "localhost" 5433 "admin" "admin" "cvdb"


getNavPoints :: ExceptT LibError IO (Either Text (Vector NavPoint))
getNavPoints = 
    getConn >>= withExceptT QueryError . ExceptT . Session.run getNavPointsSession

handleDataError :: Monad m => ExceptT LibError m (Either Text a) -> ExceptT LibError m a
handleDataError x =
    x >>= (withExceptT DataParseError . liftEither) 

-- mapServerError :: Either LibError a -> Either ServerError a
-- mapServerError (Left e) = Left $ err400 { errBody = "Error: " <> (encodeUtf8 . pack . show) e  }
-- mapServerError (Right x) = Right x

navPoints :: Handler (Vector NavPoint)
navPoints = do
    result <- liftIO $ runExceptT $ handleDataError getNavPoints
    case result of
        Left e -> throwError $ err400 { errBody = "Error: " <> (encodeUtf8 . pack . show) e  }
        Right v -> return v
    pure empty


instance FromMultipart Mem [NavPoint] where
    fromMultipart form =
        let
            fls = left show . parseLines . decodeUtf8 . fdPayload <$> files form
            parseLines = parse navPointLinesParser "Multipoart form data"
        in
            concat <$> sequence fls

upload :: [NavPoint] -> Handler String
upload navPoints = do
    liftIO $ print navPoints
    pure ""


type API =
    "users" :> Get '[JSON] [Entity User]
    :<|> "upload" :> MultipartForm Mem [NavPoint] :> Post '[PlainText] String
    :<|> "postUser" :> ReqBody '[JSON] Int :> Post '[JSON] ()
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
    users
    :<|> upload
    :<|> postUser
    :<|> navPoints

