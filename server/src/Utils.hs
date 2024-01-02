module Utils where

import Codec.Compression.GZip (CompressParams (compressLevel))
import Codec.Compression.GZip qualified as GZip
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

{-# INLINE writeFileCompressed #-}
writeFileCompressed :: (MonadIO m) => FilePath -> LByteString -> m ()
writeFileCompressed path x = do
  writeFileLBS path $ GZip.compressWith compressionParams x
  where
    compressionParams :: CompressParams
    compressionParams = GZip.defaultCompressParams {compressLevel = GZip.bestCompression}

-- {-# INLINE readJsonFileCompressed #-}
-- readJsonFileCompressed :: (Aeson.FromJSON a, MonadIO m, MonadError Woof m) => FilePath -> m a
-- readJsonFileCompressed path = do
--   compressedBS <- readFileLBS path
--   let decompressedBS = GZip.decompress compressedBS
--   case Aeson.decode' decompressedBS of
--     Just a -> pure a
--     Nothing -> throwError $ ErrorForDevelopers "Oh sorrey bad BS"
