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
import Network.Wai ( Application )
import Network.Wai.Handler.Warp ( run )
import Servant
import Control.Monad.IO.Class
    ( MonadIO(liftIO) )
import Control.Monad.Reader
import Control.Monad.Writer
import Data.Time (UTCTime(UTCTime))
import Control.Monad.Logger (NoLoggingT)
import Data.Char (toUpper)
import GHC.Generics (Generic)
import NavPoint (NavPoint)
import qualified Hasql.Connection as Connection
import qualified Hasql.Session as Session
-- import Session (getNavPointsSession)
import Data.Vector (Vector)
import Control.Monad.Except (withExceptT)
import Data.Text.Lazy (pack)




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
users = pure []

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


-- getPoints :: ExceptT LibError IO (Vector NavPoint)
-- getPoints
--     = getConn
--     >>= (withExceptT QueryError . ExceptT . Session.run getNavPointsSession)


navPoints :: Handler (Vector NavPoint)
navPoints = do
    -- result <- liftIO $ runExceptT getPoints
    -- case result of
    --     Left e -> throwError $ err400 { errBody = "Error: " <> (encodeUtf8 . pack . show) e  }
    --     Right v -> return v
    pure empty

type API =
    "users" :> Get '[JSON] [Entity User]
    -- :<|> "navpoints" :> Get '[JSON] (Vector NavPoint)

startApp :: IO ()
startApp = run 8080 app

app :: Application
app = serve api server

api :: Proxy API
api = Proxy


server :: Server API
server = users -- :<|> navPoints

