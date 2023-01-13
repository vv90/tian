{-# LANGUAGE BinaryLiterals #-}

module GeoTiff.LZW where

import Relude
import Data.List (elemIndex, tails)
import Data.Maybe (fromJust)
import Data.Binary (Word8, Word16)
import Data.Bits (shiftL, (.&.), shiftR, (.|.))
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Conduit (ConduitT, await, yield, yieldMany, (.|), sinkList, runConduit, sinkVector)
import GHC.ByteOrder (ByteOrder (..))
import Data.Vector (Vector, (!?))
import qualified Data.Vector as Vector

takeBits :: Int -> [Word8] -> (Word16, [Word8])
takeBits n (c0:c1:stream) =
    let 
        chunk :: Word16
        chunk = (fromIntegral c0 `shiftL` 8) .|. fromIntegral c1

        mask = (0xFF::Word16) `shiftR` (16 - n)
    in
        ( (fromIntegral chunk `shiftL` (16 - n)) .&. mask
        , stream
        )

bitsC :: (Monad m) => ConduitT Word8 Word8 m ()
bitsC = do
    whenJustM await $
        \byte -> do
            yield $ (byte .&. 0b10000000) `shiftR` 7
            yield $ (byte .&. 0b01000000) `shiftR` 6
            yield $ (byte .&. 0b00100000) `shiftR` 5
            yield $ (byte .&. 0b00010000) `shiftR` 4
            yield $ (byte .&. 0b00001000) `shiftR` 3
            yield $ (byte .&. 0b00000100) `shiftR` 2
            yield $ (byte .&. 0b00000010) `shiftR` 1
            yield $ byte .&. 0b00000001
            bitsC

decodeC :: (Monad m) => ConduitT Word8 Word8 m ()
decodeC = 
    let 
        initTable :: Vector (NonEmpty Word8)
        initTable = Vector.fromList $ 
            [ x:|[] | x <- [0..255] ] ++ [ 0:|[] , 0:|[] ]

        awaitBits n = do
            bits <- replicateM n await
            case sequence bits of
                Just bs -> 
                    return $ foldl' (\acc bit -> (acc `shiftL` 1) .|. fromIntegral bit) (0::Int) bs
                Nothing ->
                    error "not enough bits"

        codeSize tableSize
            | tableSize < 511 = 9
            | tableSize < 1023 = 10
            | tableSize < 2047 = 11
            | otherwise = 12

        consumeCode table lastVal = do
            code <- awaitBits $ codeSize $ Vector.length table
            -- print $ "code " <> show code
            case (code, table !? code) of
                (256, _) -> start
                (257, _) -> pure ()
                (c, Just val) -> do
                    yieldMany val
                    let newVal = lastVal <> (fromIntegral (head val):|[])
                    consumeCode (Vector.snoc table newVal) val
                (c, Nothing) -> do
                    let res = lastVal <> (head lastVal :| [])
                    yieldMany res
                    consumeCode (Vector.snoc table res) res

        start = do
            code <- awaitBits 9
            case (code, initTable !? code) of
                (256, _) -> start
                (257, _) -> pure ()
                (c, Just val) -> do
                    yieldMany val
                    consumeCode initTable val
                (c, Nothing) -> do
                    error $ "Code " <> show c <> " not found in the initial table"
    in do
        start

        
decodeLZW :: ByteOrder -> [Word8] -> ExceptT String IO (Vector Word16)
decodeLZW bo input = do
    print "decoding"
    runConduit $
        yieldMany input .| bitsC .| decodeC .| packWordsC bo .| sinkVector

packWordsC :: (Monad m) => ByteOrder -> ConduitT Word8 Word16 m ()
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

        (_, Just b0', Nothing) -> 
            error "Odd number of bytes in the input stream"

        _ -> pure ()

