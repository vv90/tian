module Utils where

import Control.Exception (try)
import Control.Monad.Except (withExceptT)
import Data.Vector (Vector)
import Data.Vector as V (cons, drop, empty, take)
import Relude

catchLiftIO :: IO a -> ExceptT Text IO a
catchLiftIO = withExceptT transformError . ExceptT . try
  where
    transformError :: SomeException -> Text
    transformError = show

unflattenVector :: Int -> Vector a -> Vector (Vector a)
unflattenVector rowLength items
  | rowLength <= 0 = V.empty
  | not (null items) = V.take rowLength items `V.cons` unflattenVector rowLength (V.drop rowLength items)
  | otherwise = V.empty
