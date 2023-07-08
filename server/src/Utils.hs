module Utils where

import Control.Exception (try)
import Control.Monad.Except (withExceptT)
import Relude

catchLiftIO :: IO a -> ExceptT Text IO a
catchLiftIO = withExceptT transformError . ExceptT . try
  where
    transformError :: SomeException -> Text
    transformError = show
