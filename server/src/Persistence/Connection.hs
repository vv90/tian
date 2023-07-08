module Persistence.Connection where

import Control.Monad.Except (liftEither, withExceptT)
import Hasql.Connection qualified as Connection
import Relude
import System.Environment (getEnv)
import Utils (catchLiftIO)

getConnection :: ExceptT Text IO Connection.Connection
getConnection =
  let tryGetEnv = catchLiftIO . getEnv
      conn host port usr pwd db =
        withExceptT show
          $ ExceptT
          $ Connection.acquire
          $ Connection.settings host port usr pwd db
   in do
        host <- tryGetEnv "DB_HOST"
        port <- tryGetEnv "DB_PORT" >>= liftEither . readEither @Word16
        usr <- tryGetEnv "DB_USER"
        pwd <- tryGetEnv "DB_PASS"
        db <- tryGetEnv "DB_NAME"

        conn (encodeUtf8 host) port (encodeUtf8 usr) (encodeUtf8 pwd) (encodeUtf8 db)
