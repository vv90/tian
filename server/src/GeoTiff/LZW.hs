module GeoTiff.LZW where

import Conduit (ConduitT, await, runConduit, sinkVector, yield, yieldMany, (.|))
import Data.Bits (shiftL, shiftR, (.&.), (.|.))
import Data.Vector (Vector, (!?))
import Data.Vector qualified as Vector
import GHC.ByteOrder (ByteOrder (..))
import Relude

bitsC :: (Monad m) => ConduitT Word8 Word8 m ()
bitsC = do
  whenJustM await
    $ \byte -> do
      yield $ (byte .&. 0b10000000) `shiftR` 7
      yield $ (byte .&. 0b01000000) `shiftR` 6
      yield $ (byte .&. 0b00100000) `shiftR` 5
      yield $ (byte .&. 0b00010000) `shiftR` 4
      yield $ (byte .&. 0b00001000) `shiftR` 3
      yield $ (byte .&. 0b00000100) `shiftR` 2
      yield $ (byte .&. 0b00000010) `shiftR` 1
      yield $ byte .&. 0b00000001
      bitsC

decodeC :: (MonadIO m, MonadFail m) => ConduitT Word8 Word8 m ()
decodeC =
  let initTable :: Vector (NonEmpty Word8)
      initTable =
        Vector.fromList
          $ [x :| [] | x <- [0 .. 255]]
          ++ [0 :| [], 0 :| []]

      awaitBits (n, s) = do
        bits <- replicateM n await
        case sequence bits of
          Just bs ->
            return . Just $ foldl' (\acc bit -> (acc `shiftL` 1) .|. fromIntegral bit) (0 :: Int) bs
          Nothing -> do
            -- pure ()
            putStrLn $ "Table size: " <> show s
            putStrLn $ "bits: " <> show bits
            -- fail $ "Not enough bits in the input stream. Need " <> show n <> " , got " <> show (filter isJust bits & length)
            return Nothing

      codeSize tableSize
        | tableSize < 511 = (9, tableSize)
        | tableSize < 1023 = (10, tableSize)
        | tableSize < 2047 = (11, tableSize)
        | otherwise = (12, tableSize)

      consumeCode table lastVal = do
        code <- awaitBits $ codeSize $ Vector.length table
        -- print $ "code " <> show code
        -- print $ "table size " <> show (Vector.length table)
        case (code, code >>= (table !?)) of
          (Nothing, _) -> pass
          (Just 256, _) -> do
            -- print "start"
            start
          (Just 257, _) -> pass
          (Just _c, Just val) -> do
            yieldMany val
            let newVal = lastVal <> (head val :| [])
            consumeCode (Vector.snoc table newVal) val
          (Just _c, Nothing) -> do
            let res = lastVal <> (head lastVal :| [])
            yieldMany res
            consumeCode (Vector.snoc table res) res

      start :: (MonadIO m, MonadFail m) => ConduitT Word8 Word8 m ()
      start = do
        code <- awaitBits (9, Vector.length initTable)
        case (code, code >>= (initTable !?)) of
          (Nothing, _) -> pass
          (Just 256, _) -> start
          (Just 257, _) -> pass
          (Just _c, Just val) -> do
            yieldMany val
            consumeCode initTable val
          (Just c, Nothing) -> do
            fail $ "Code " <> show c <> " not found in the initial table"
   in do
        start

decodeLZW :: ByteOrder -> Vector Word8 -> ExceptT String IO (Vector Word16)
decodeLZW bo input = do
  -- print "decoding"
  runConduit
    $ yieldMany input
    .| bitsC
    .| decodeC
    .| packWordsC bo
    .| sinkVector

watchC :: (MonadIO m, Show a) => ConduitT a a m ()
watchC = do
  whenJustM await $ \a -> do
    liftIO $ print a
    yield a
    -- getLine
    watchC

packWordsC :: (MonadIO m, MonadFail m) => ByteOrder -> ConduitT Word8 Word16 m ()
packWordsC byteOrder = do
  b0 <- await
  b1 <- await

  case (byteOrder, b0, b1) of
    (LittleEndian, Just b0', Just b1') -> do
      yield $ fromIntegral b1' `shiftL` 8 .|. fromIntegral b0'
      packWordsC byteOrder
    (BigEndian, Just b0', Just b1') -> do
      yield $ fromIntegral b0' `shiftL` 8 .|. fromIntegral b1'
      packWordsC byteOrder
    (LittleEndian, Just b0', Nothing) -> do
      -- fail "Odd number of bytes in the input stream"
      yield $ fromIntegral b0'
      pass
    (BigEndian, Just b0', Nothing) -> do
      yield $ fromIntegral b0' `shiftL` 8
      pass
    _ -> pass
