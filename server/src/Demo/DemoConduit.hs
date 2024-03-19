module Demo.DemoConduit where

import Conduit (ConduitT, ResourceT, await, bracketP, leftover, runConduitRes, yield, (.|))
import Control.Concurrent (forkIO, killThread, threadDelay)
import Control.Concurrent.STM.TBMQueue (closeTBMQueue, newTBMQueue, readTBMQueue)
import Data.Conduit.TQueue (sinkTBMQueue)
import Relude
import System.Directory (getDirectoryContents)
import System.FilePath (takeFileName)
import Text.Parsec (Parsec, alphaNum, char, many1, parse, string)

data TrackFileInfo = TrackFileInfo
  { dateStr :: Text,
    callsign :: Text
  }
  deriving stock (Show)

trackFileInfoParser :: Parsec Text () TrackFileInfo
trackFileInfoParser =
  (TrackFileInfo . toText <$> many1 alphaNum)
    <* char '_'
    <*> (toText <$> many1 alphaNum)
    <* string ".igc"

findTrackFiles :: FilePath -> IO [(TrackFileInfo, FilePath)]
findTrackFiles filePath =
  let parseTrackFileInfo fp =
        (,fp) <$> parse trackFileInfoParser "" (toText $ takeFileName fp)
   in do
        files <- rightToMaybe . parseTrackFileInfo <<$>> getDirectoryContents filePath
        putStrLn
          $ show (length $ catMaybes files)
          <> " recognized track files"

        pure $ catMaybes files

-- demoAprsSource :: TrackFileInfo -> FilePath -> ConduitT () AprsMessage (ResourceT IO) ()
-- demoAprsSource info path = do
--   sourceFile path
--     .| decodeUtf8LenientC
--     .| mapOutputMaybe
--       (toFlightInfo (show path) >=> toAprsMessage info.callsign)
--       linesUnboundedC
--   where
--     toFlightInfo :: Text -> Text -> Maybe FlightInfo
--     toFlightInfo compId =
--       rightToMaybe . parse flightInfoParser (show compId)

--     toAprsMessage :: Text -> FlightInfo -> Maybe AprsMessage
--     toAprsMessage msgId fi =
--       case fi of
--         Fix tp ->
--           Just
--             $ AprsMessage
--               { source = msgId,
--                 time = tp.time,
--                 lat = tp.lat,
--                 lon = tp.lon,
--                 elev = tp.altitudeGps
--               }
--         _ -> Nothing

playbackC :: (MonadIO m) => (a -> Int) -> Int -> ConduitT a a m ()
playbackC getTime speed =
  let stepMillis = 100 :: Int

      play time =
        whenJustM await $ \msg ->
          if time >= getTime msg
            then do
              yield msg
              play time
            else do
              leftover msg
              liftIO $ threadDelay (1000 * stepMillis)
              play $ time + (stepMillis * speed)
   in whenJustM await $ \msg ->
        yield msg >> play (getTime msg)

extractSmallest :: (Ord b) => (a -> b) -> NonEmpty a -> (a, [a])
extractSmallest f (x :| xs) =
  let extract [] x' xs' =
        (x', xs')
      extract (y : ys) z zs =
        if f y < f z
          then extract ys y (z : zs)
          else extract ys z (y : zs)
   in extract xs x []

mergeSourcesOn :: (Ord b) => (a -> b) -> [ConduitT () a (ResourceT IO) ()] -> ConduitT () a (ResourceT IO) ()
mergeSourcesOn f sources =
  -- we have a list of data conduits where data in each conduit is sorted
  -- but the they are not sorted with respect to each other

  -- we need to merge them into a single conduit where data is sorted
  let makeSinkQueue c = do
        q <- atomically $ newTBMQueue 100
        pure (q, c)

      feedSinkQueue q c =
        runConduitRes
          $ bracketP
            pass
            (\_ -> atomically $ closeTBMQueue q) -- make sure to close the queue when the conduit is done
            (\_ -> c .| sinkTBMQueue q) -- feed the queue with data from the conduit
      readQ q = do
        v <- atomically $ readTBMQueue q
        pure $ (,q) <$> v

      consumeQueues qs = do
        -- the structure is (`last extracted value`, `queue containing the rest of the values`)
        -- pick the smallest of the extracted values with it's corresponding queue
        let m = viaNonEmpty (extractSmallest (\(v, _q) -> f v)) qs
        whenJust m $ \((v, q), rest) -> do
          yield v
          -- read the next value from the queue which produced the smallest value last time
          next <- readQ q
          case next of
            Just x -> do
              consumeQueues $ x : rest
            Nothing -> do
              consumeQueues rest
   in do
        -- make a list of tuples (queue, conduit)
        qs <- traverse (liftIO . makeSinkQueue) sources

        bracketP
          (liftIO $ traverse (forkIO . uncurry feedSinkQueue) qs) -- fork a thread for each conduit to feed it's queue
          (liftIO . traverse_ killThread) -- kill the threads when the conduit is done
          ( \_tids -> do
              qs' <- catMaybes <$> traverse (readQ . fst) qs -- read the first value from each queue
              consumeQueues qs'
          )
