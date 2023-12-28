module GeoTiff.Tiff where

import Conduit (ConduitT, awaitForever, runConduit, withSinkFile, yield, yieldMany, (.|))
import Control.Monad.Except (liftEither)
import Data.Binary.Get (Get, getDoublebe, getDoublele, getWord16be, getWord16le, getWord32be, getWord32le, getWord8, runGetOrFail)
import Data.Binary.Put (Put, putWord16be, putWord16le, runPut)
import Data.ByteString.Lazy qualified as LBS
import Data.Vector (Vector, (!), (!?))
import Data.Vector qualified as Vector
import GHC.ByteOrder (ByteOrder (..))
import GHC.IO.Handle (hSeek)
import Geo (Latitude (..), Longitude (..))
import GeoTiff.ElevationPoint (ElevationPoint (ElevationPoint))
import GeoTiff.LZW (decodeLZW)
import Relude
import System.IO (SeekMode (AbsoluteSeek), hPutStrLn, hTell, openBinaryFile, withBinaryFile)

data DataType
  = Byte Word8 -- 1
  | Ascii Char -- 2
  | Short Word16 -- 3
  | Long Word32 -- 4
  | URational (Word32, Word32) -- 5
  | SByte Int8 -- 6
  | Undefined Word8 -- 7
  | SShort Int16 -- 8
  | SLong Int32 -- 9
  | SRational (Int32, Int32) -- 10
  | Float Float -- 11
  | Double Double -- 12

-- data Tag
--     = BitsPerSample Int -- 258
--     | Compression Compression -- 259
--     | ImageWidth Int -- 256
--     | ImageHeight Int -- 257
--     | PhotometricInterpretation PhotometricInterpretation-- 262
--     | SamplesPerPixel Int-- 277
--     | PlanarConfiguration PlanarConfiguration-- Word16-- 284
--     | Predictor Predictor -- 317
--     | TileWidth Int-- 322
--     | TileHeight Int-- 323
--     | TileOffsets Int Integer -- [Int]-- 324
--     | TileByteCounts Int Integer -- [Int]-- 325
--     | SampleFormat Word16-- 339
--     | ModelPixelScale PixelScale-- 33550
--     | ModelTiepoint Int Integer -- TiePoint -- 33922
--     | GeoKeyDirectory Int Integer -- [Word16]-- 34735
--     | GeoDoubleParams Int Integer -- [Double]-- 34736
--     | GeoAsciiParams Int Integer -- String -- 34737
--     | GDALMetadata Int Integer -- String -- 42112
--     deriving (Show)

-- The number of columns in the image, i.e., the number of pixels per row.
newtype ImageWidth = ImageWidth Int -- 256
  deriving stock (Show)

-- The number of rows of pixels in the image.
newtype ImageHeight = ImageHeight Int -- 257
  deriving stock (Show)

newtype BitsPerSample = BitsPerSample Int -- 258
  deriving stock (Show)

data Compression = NoCompression | LZW -- 259
  deriving stock (Show)

data PhotometricInterpretation = BlackIsZero -- 262
  deriving stock (Show)

data SamplesPerPixel = OneSample -- Int -- 277
  deriving stock (Show)

data PlanarConfiguration = Chunky -- 284
  deriving stock (Show)

data Predictor = PredictorNone -- 317
  deriving stock (Show)

-- The tile width in pixels. This is the number of columns in each tile.
newtype TileWidth = TileWidth Int -- 322
  deriving stock (Show)

-- The tile length (height) in pixels. This is the number of rows in each tile.
newtype TileHeight = TileHeight Int -- 323
  deriving stock (Show)

-- Assuming integer arithmetic, three computed values that are useful in the following field descriptions are:
-- TilesAcross = (ImageWidth + TileWidth - 1) / TileWidth
-- TilesDown = (ImageLength + TileLength - 1) / TileLength
-- TilesPerImage = TilesAcross * TilesDown

newtype TileOffsets = TileOffsets (Vector Integer) -- 324
  deriving stock (Show)

newtype TileByteCounts = TileByteCounts (Vector Integer) -- 325
  deriving stock (Show)

data SampleFormat = UnsignedInteger | SignedInteger -- 339
  deriving stock (Show)

type ScaleX = Double

type ScaleY = Double

type ScaleZ = Double

type PixelScale = (ScaleX, ScaleY, ScaleZ)

-- This tag may be used to specify the size of raster pixel spacing in the model space units,
-- when the raster space can be embedded in the model space coordinate system without rotation, and consists of the following 3 values:
-- ModelPixelScaleTag = (ScaleX, ScaleY, ScaleZ)
-- where ScaleX and ScaleY give the horizontal and vertical spacing of raster pixels.
-- The ScaleZ is primarily used to map the pixel value of a digital elevation model into the correct Z-scale, and so for most other purposes this value should be zero (since most model spaces are 2-D, with Z=0).
newtype ModelPixelScale = ModelPixelScale PixelScale -- 33550
  deriving stock (Show)

type RasterX = Double

type RasterY = Double

type RasterZ = Double

type ModelX = Double

type ModelY = Double

type ModelZ = Double

type TiePoint = (RasterX, RasterY, RasterZ, ModelX, ModelY, ModelZ)

-- This tag stores raster->model tiepoint pairs in the order
-- ModelTiepointTag = (...,I,J,K, X,Y,Z...)
-- where (I,J,K) is the point at location (I,J) in raster space with pixel-value K, and (X,Y,Z) is a vector in model space
newtype ModelTiePoint = ModelTiePoint TiePoint -- 33922
  deriving stock (Show)

data GeoKey
  = GTModelType ModelType -- 1024
  | GTRasterType RasterType -- 1025
  | GeographicType GeographicType -- Word16 -- 2048
  | GeogCitation String -- 2049
  | GeogAngularUnits AngularUnits -- 2054
  | GeogSemiMajorAxis Double -- 2057
  | GeogInvFlattening Double -- 2059
  deriving stock (Show)

data ModelType = Geographic
  deriving stock (Show)

data RasterType = PixelIsArea | PixelIsPoint
  deriving stock (Show)

data GeographicType = WGS_84
  deriving stock (Show)

data AngularUnits = Degree
  deriving stock (Show)

data TiffConfig = TiffConfig
  { imageWidth :: ImageWidth,
    imageHeight :: ImageHeight,
    bitsPerSample :: BitsPerSample,
    compression :: Compression,
    photometricInterpretation :: PhotometricInterpretation,
    samplesPerPixel :: SamplesPerPixel,
    planarConfiguration :: PlanarConfiguration,
    predictor :: Predictor,
    tileWidth :: TileWidth,
    tileHeight :: TileHeight,
    tileOffsets :: TileOffsets,
    tileByteCounts :: TileByteCounts,
    sampleFormat :: SampleFormat,
    modelPixelScale :: ModelPixelScale,
    modelTiePoint :: ModelTiePoint
  }
  deriving stock (Show)

-- readElevationPoints :: [MapTile] -> ExceptT String IO [[Int]]
-- readElevationPoints tiles = do

readConfig :: ByteOrder -> Handle -> ExceptT String IO TiffConfig
readConfig bo h =
  let imageWidth :: Word16 -> Word16 -> Word32 -> Word32 -> Either String ImageWidth
      imageWidth 256 _ 1 value =
        Right $ ImageWidth $ fromIntegral value
      imageWidth 256 _ count _ =
        Left $ "ImageWidth: invalid dataCount: " <> show count
      imageWidth tid _ _ _ =
        Left $ "ImageWidth: invalid tagId: " <> show tid <> ", expected 256"

      imageHeight :: Word16 -> Word16 -> Word32 -> Word32 -> Either String ImageHeight
      imageHeight 257 _ 1 value =
        Right $ ImageHeight $ fromIntegral value
      imageHeight 257 _ count _ =
        Left $ "ImageHeight: invalid dataCount: " <> show count
      imageHeight tid _ _ _ =
        Left $ "ImageHeight: invalid tagId: " <> show tid <> ", expected 257"

      bitsPerSample :: Word16 -> Word16 -> Word32 -> Word32 -> Either String BitsPerSample
      bitsPerSample 258 _ 1 value =
        Right $ BitsPerSample $ fromIntegral value
      bitsPerSample 258 _ count _ =
        Left $ "BitsPerSample: invalid dataCount: " <> show count
      bitsPerSample tid _ _ _ =
        Left $ "BitsPerSample: invalid tagId: " <> show tid <> ", expected 258"

      compression :: Word16 -> Word16 -> Word32 -> Word32 -> Either String Compression
      compression 259 _ 1 1 =
        Right NoCompression
      compression 259 _ 1 5 =
        Right LZW
      compression 259 _ 1 value =
        Left $ "Compression: unsupported compression type: " <> show value
      compression 259 3 count _ =
        Left $ "Compression: invalid dataCount: " <> show count <> ", expected 1"
      compression tid _ _ _ =
        Left $ "Compression: invalid tagId: " <> show tid <> ", expected 259"

      photometricInterpretation :: Word16 -> Word16 -> Word32 -> Word32 -> Either String PhotometricInterpretation
      photometricInterpretation 262 _ 1 1 =
        Right BlackIsZero
      photometricInterpretation 262 _ 1 value =
        Left $ "PhotometricInterpretation: unsupported value: " <> show value
      photometricInterpretation 262 _ count _ =
        Left $ "PhotometricInterpretation: invalid dataCount: " <> show count <> ", expected 1"
      photometricInterpretation tid _ _ _ =
        Left $ "PhotometricInterpretation: invalid tagId: " <> show tid <> ", expected 262"

      samplesPerPixel :: Word16 -> Word16 -> Word32 -> Word32 -> Either String SamplesPerPixel
      samplesPerPixel 277 _ 1 1 =
        Right OneSample
      samplesPerPixel 277 _ 1 value =
        Left $ "SamplesPerPixel: unsupported value: " <> show value
      samplesPerPixel 277 _ count _ =
        Left $ "SamplesPerPixel: invalid dataCount: " <> show count <> ", expected 1"
      samplesPerPixel tid _ _ _ =
        Left $ "SamplesPerPixel: invalid tagId: " <> show tid <> ", expected 277"

      planarConfiguration :: Word16 -> Word16 -> Word32 -> Word32 -> Either String PlanarConfiguration
      planarConfiguration 284 _ 1 1 =
        Right Chunky
      planarConfiguration 284 _ 1 value =
        Left $ "PlanarConfiguration: unsupported value: " <> show value
      planarConfiguration 284 _ count _ =
        Left $ "PlanarConfiguration: invalid dataCount: " <> show count <> ", expected 1"
      planarConfiguration tid _ _ _ =
        Left $ "PlanarConfiguration: invalid tagId: " <> show tid <> ", expected 284"

      predictor :: Word16 -> Word16 -> Word32 -> Word32 -> Either String Predictor
      predictor 317 _ 1 1 =
        Right PredictorNone
      predictor 317 _ 1 value =
        Left $ "Predictor: unsupported value: " <> show value
      predictor 317 _ count _ =
        Left $ "Predictor: invalid dataCount: " <> show count <> ", expected 1"
      predictor tid _ _ _ =
        Left $ "Predictor: invalid tagId: " <> show tid <> ", expected 317"

      tileWidth :: Word16 -> Word16 -> Word32 -> Word32 -> Either String TileWidth
      tileWidth 322 _ 1 value =
        Right $ TileWidth $ fromIntegral value
      tileWidth 322 _ count _ =
        Left $ "TileWidth: invalid dataCount: " <> show count
      tileWidth tid _ _ _ =
        Left $ "TileWidth: invalid tagId: " <> show tid <> ", expected 322"

      tileHeight :: Word16 -> Word16 -> Word32 -> Word32 -> Either String TileHeight
      tileHeight 323 _ 1 value =
        Right $ TileHeight $ fromIntegral value
      tileHeight 323 _ count _ =
        Left $ "TileHeight: invalid dataCount: " <> show count
      tileHeight tid _ _ _ =
        Left $ "TileHeight: invalid tagId: " <> show tid <> ", expected 323"

      tileOffsets :: Word16 -> Word16 -> Word32 -> Word32 -> Either String (Int, Integer)
      tileOffsets 324 4 count offset =
        Right (fromIntegral count, fromIntegral offset)
      tileOffsets 324 dt _count _ =
        Left $ "TileOffsets: invalid dataType: " <> show dt <> ", expected 4"
      tileOffsets tid _ _ _ =
        Left $ "TileOffsets: invalid tagId: " <> show tid <> ", expected 324"

      tileByteCounts :: Word16 -> Word16 -> Word32 -> Word32 -> Either String (Int, Integer)
      tileByteCounts 325 4 count offset =
        Right (fromIntegral count, fromIntegral offset)
      tileByteCounts 325 dt _count _ =
        Left $ "TileByteCounts: invalid dataType: " <> show dt <> ", expected 4"
      tileByteCounts tid _ _ _ =
        Left $ "TileByteCounts: invalid tagId: " <> show tid <> ", expected 325"

      sampleFormat :: Word16 -> Word16 -> Word32 -> Word32 -> Either String SampleFormat
      sampleFormat 339 _ 1 1 =
        Right UnsignedInteger
      sampleFormat 339 _ 1 2 =
        Right SignedInteger
      sampleFormat 339 _ 1 value =
        Left $ "SampleFormat: unsupported value: " <> show value
      sampleFormat 339 _ count _ =
        Left $ "SampleFormat: invalid dataCount: " <> show count <> ", expected 1"
      sampleFormat tid _ _ _ =
        Left $ "SampleFormat: invalid tagId: " <> show tid <> ", expected 339"

      modelPixelScale :: Word16 -> Word16 -> Word32 -> Word32 -> Either String Integer
      modelPixelScale 33550 12 3 offset =
        Right $ fromIntegral offset
      modelPixelScale 33550 12 count _ =
        Left $ "ModelPixelScale: invalid dataCount: " <> show count <> ", expected 3"
      modelPixelScale 33550 dt _ _ =
        Left $ "ModelPixelScale: invalid dataType: " <> show dt <> ", expected 12"
      modelPixelScale tid _ _ _ =
        Left $ "ModelPixelScale: invalid tagId: " <> show tid <> ", expected 33550"

      modelTiePoint :: Word16 -> Word16 -> Word32 -> Word32 -> Either String Integer
      modelTiePoint 33922 12 6 offset =
        Right $ fromIntegral offset
      modelTiePoint 33922 12 count _ =
        Left $ "ModelTiePoint: invalid dataCount: " <> show count <> ", expected 6"
      modelTiePoint 33922 dt _ _ =
        Left $ "ModelTiePoint: invalid dataType: " <> show dt <> ", expected 12"
      modelTiePoint tid _ _ _ =
        Left $ "ModelTiePoint: invalid tagId: " <> show tid <> ", expected 33922"

      readTileOffsetsData :: (Int, Integer) -> ExceptT String IO TileOffsets
      readTileOffsetsData (count, offset) =
        TileOffsets . Vector.fromList . fmap fromIntegral <$> readTagData (readWord32Many count) offset

      readTileByteCountsData :: (Int, Integer) -> ExceptT String IO TileByteCounts
      readTileByteCountsData (count, offset) =
        TileByteCounts . Vector.fromList . fmap fromIntegral <$> readTagData (readWord32Many count) offset

      readModelPixelScaleData :: Integer -> ExceptT String IO ModelPixelScale
      readModelPixelScaleData offset = do
        [x, y, z] <- readTagData (readDoubleMany 3) offset
        pure $ ModelPixelScale (x, y, z)

      readModelTilePointData :: Integer -> ExceptT String IO ModelTiePoint
      readModelTilePointData offset = do
        [x, y, z, i, j, k] <- readTagData (readDoubleMany 6) offset
        pure $ ModelTiePoint (x, y, z, i, j, k)

      readTagData getDataFn offset = do
        hPos <- liftIO $ hTell h
        liftIO $ hSeek h AbsoluteSeek offset
        result <- getDataFn bo h
        liftIO $ hSeek h AbsoluteSeek hPos
        pure result

      readTag :: (Word16 -> Word16 -> Word32 -> Word32 -> Either String a) -> ExceptT String IO a
      readTag decodeTag = do
        tagId <- readWord16 bo h
        tagType <- readWord16 bo h
        dataCount <- readWord32 bo h
        dataValue <- readWord32 bo h
        liftEither $ decodeTag tagId tagType dataCount dataValue
   in TiffConfig
        <$> readTag imageWidth
        <*> readTag imageHeight
        <*> readTag bitsPerSample
        <*> readTag compression
        <*> readTag photometricInterpretation
        <*> readTag samplesPerPixel
        <*> readTag planarConfiguration
        <*> readTag predictor
        <*> readTag tileWidth
        <*> readTag tileHeight
        <*> (readTag tileOffsets >>= readTileOffsetsData)
        <*> (readTag tileByteCounts >>= readTileByteCountsData)
        <*> readTag sampleFormat
        <*> (readTag modelPixelScale >>= readModelPixelScaleData)
        <*> (readTag modelTiePoint >>= readModelTilePointData)

readTileElevations :: ByteOrder -> Handle -> (Integer, Integer) -> ExceptT String IO (Vector Word16)
readTileElevations byteOrder h (offset, len) = do
  liftIO $ hSeek h AbsoluteSeek offset
  readWord8Many (fromIntegral len) h >>= decodeLZW byteOrder

readTiffElevationData :: FilePath -> ExceptT String IO [Vector ElevationPoint]
readTiffElevationData filePath =
  do
    h <- liftIO $ openBinaryFile filePath ReadMode
    (byteOrder, ifdOffset) <- ExceptT $ decodeHeader <$> LBS.hGet h 8

    liftIO $ hSeek h AbsoluteSeek ifdOffset
    _numTags <- readWord16 byteOrder h
    config <- readConfig byteOrder h

    let TileOffsets offsets = config.tileOffsets
        TileByteCounts byteCounts = config.tileByteCounts
        ImageWidth imageWidth = config.imageWidth
        ImageHeight imageHeight = config.imageHeight
        TileWidth tileWidth = config.tileWidth
        TileHeight tileHeight = config.tileHeight
        ModelTiePoint _tiePoint@(_px, _py, _pz, mLon, mLat, _mz) = config.modelTiePoint
        ModelPixelScale _pixelScale@(xScale, yScale, _zScale) = config.modelPixelScale

        -- width of the image in tiles (rounded up to cover the whole image)
        tilesCountRow :: Int
        tilesCountRow = (ceiling @Double @Int) $ fromIntegral imageWidth / fromIntegral tileWidth
        -- height of the image in tiles (rounded up to cover the whole image)
        -- tilesCountCol :: Int
        -- tilesCountCol = (ceiling @Double @Int) $ fromIntegral imageHeight / fromIntegral tileHeight

        -- row of the tile in the image
        tileImageRow :: Int -> Int
        tileImageRow tileIndex = tileIndex `div` tilesCountRow

        -- column of the tile in the image
        tileImageCol :: Int -> Int
        tileImageCol tileIndex = tileIndex `mod` tilesCountRow

        -- row of the pixel in the tile
        pixelTileRow :: Int -> Int
        pixelTileRow pixelIndex = pixelIndex `div` tileWidth

        -- column of the pixel in the tile
        pixelTileCol :: Int -> Int
        pixelTileCol pixelIndex = pixelIndex `mod` tileWidth

        -- the coordinates of the tile pixel in the image
        pixelImageCoord :: Int -> Int -> (Int, Int)
        pixelImageCoord tile pixel =
          ( tileImageCol tile * tileWidth + pixelTileCol pixel,
            tileImageRow tile * tileHeight + pixelTileRow pixel
          )

        pixelToElevationPoint :: Int -> Int -> Word16 -> Maybe ElevationPoint
        pixelToElevationPoint tile pixel elevation =
          let (pixelCol, pixelRow) = pixelImageCoord tile pixel
           in if pixelCol >= imageWidth || pixelRow >= imageHeight
                then Nothing
                else
                  Just
                    $ ElevationPoint
                      (fromIntegral elevation)
                      (LongitudeDegrees $ fromIntegral pixelCol * xScale + mLon)
                      (LatitudeDegrees $ fromIntegral pixelRow * yScale + mLat)

        readTile :: Int -> (Integer, Integer) -> ExceptT String IO (Vector ElevationPoint)
        readTile tileIndex (offset, tileLength) =
          do
            liftIO $ hSeek h AbsoluteSeek offset

            r <- readWord8Many (fromIntegral tileLength) h >>= decodeLZW byteOrder

            pure $ Vector.imapMaybe (pixelToElevationPoint tileIndex) r

    -- pixelCoords :: Int -> Int -> Maybe (Double, Double)
    -- pixelCoords tile pixel =
    --   if tile +

    -- print tiePoint
    -- print pixelScale
    -- print (imageWidth, imageHeight)
    -- print (tileWidth, tileHeight)
    -- print $ pixelImageCoord 14 256
    r <- sequence $ Vector.imap readTile $ Vector.zip offsets byteCounts

    -- r <- readTile 0 ((Vector.unsafeHead offsets), (Vector.unsafeHead byteCounts))
    -- print $ Vector.unsafeIndex r 10
    -- print $ Vector.length $ Vector.concat $ Vector.toList r

    -- pass
    pure $ Vector.toList r

tiffChunksC :: Handle -> ConduitT () (Vector Word16) (ExceptT String IO) ()
tiffChunksC h = do
  (byteOrder, ifdOffset) <- lift $ ExceptT $ decodeHeader <$> LBS.hGet h 8

  liftIO $ hSeek h AbsoluteSeek ifdOffset
  _numTags <- lift $ readWord16 byteOrder h
  config <- lift $ readConfig byteOrder h

  let ModelTiePoint _tiePoint@(_px, _py, _pz, _mLon, _mLat, _mz) = config.modelTiePoint
      -- size of raster pixel spacing in the model space units
      ModelPixelScale _pixelScale@(_xScale, _yScale, _zScale) = config.modelPixelScale
      TileHeight modelTileHeight = config.tileHeight
      TileWidth modelTileWidth = config.tileWidth
      ImageHeight imageHeight = config.imageHeight
      ImageWidth imageWidth = config.imageWidth
      TileOffsets tileOffsets = config.tileOffsets
      TileByteCounts tileByteCounts = config.tileByteCounts

      -- width of the image in tiles (rounded up to cover the whole image)
      tilesCountRow :: Int
      tilesCountRow = (ceiling @Double @Int) $ fromIntegral imageWidth / fromIntegral modelTileWidth
      -- height of the image in tiles (rounded up to cover the whole image)
      tilesCountCol :: Int
      tilesCountCol = (ceiling @Double @Int) $ fromIntegral imageHeight / fromIntegral modelTileHeight

      getTileOffset :: Int -> Either String Integer
      getTileOffset i =
        maybeToRight ("Failed to get tile offset for tile index [" <> show i <> "]")
          $ tileOffsets
          !? i
      -- \$ tileOffsets !? (i + j * modelTileRowLength)

      getTileByteCount :: Int -> Either String Integer
      getTileByteCount i =
        maybeToRight ("Failed to get tile byte count for tile [" <> show i <> "]")
          $ tileByteCounts
          !? i
      -- \$ tileByteCounts !? (i + j * modelTileRowLength)

      -- actualTileWidth i = min modelTileWidth (imageWidth - i * modelTileWidth)
      -- actualTileHeight j = min modelTileHeight (imageHeight - j * modelTileHeight)

      readTile :: Int -> ExceptT String IO (Vector Word16)
      readTile tileIndex = do
        offset <- liftEither (getTileOffset tileIndex)
        count <- liftEither (getTileByteCount tileIndex)
        liftIO $ hSeek h AbsoluteSeek offset
        tileData <- readWord8Many (fromIntegral count) h

        decodeLZW byteOrder tileData
      -- return decoded tile values along with actual tile width and height
      -- actual tile width and height could be (and almost always is) less than `modelTileWidth` and `modelTileHeight`
      -- for the last column and last row of tiles in the image
      -- because the image width and height are not necessarily divisible by `modelTileWidth` and `modelTileHeight`
      -- but tiles still need to cover the whole image
      -- let
      --     (i, j) = tileIndex `divMod` tilesCountRow
      --     width = actualTileWidth i
      --     height = actualTileHeight j

      -- when (Vector.length values /= width * height) $
      --     fail $ "Failed to read tile [" <> show tileIndex <> "]: expected " <> show (width * height) <> " values, got " <> show (Vector.length values)
      -- print $ "Tile " <> show tileIndex <> " length: " <> show (Vector.length values)

      -- pure (width, height, values)

      -- unwraps flat tile data into 2d vector: Vector[row][col]
      _tileToRows :: Vector Word16 -> Vector (Vector Word16)
      _tileToRows tile =
        Vector.generate modelTileHeight
          $ \rowIndex -> Vector.slice (rowIndex * modelTileWidth) modelTileWidth tile

      -- appends each tile row to the corresponding row the `rows` vector
      _appendTileToRows :: Vector (Vector Word16) -> Vector Word16 -> Vector (Vector Word16)
      _appendTileToRows rows tile =
        Vector.zipWith (Vector.++) rows (_tileToRows tile)

      -- reads tile with index `tileIndex`,
      -- transforms it into rows
      -- and appends each row to corresponding row in the `rows` vector
      _foldTiles :: Vector (Vector Word16) -> Int -> ExceptT String IO (Vector (Vector Word16))
      _foldTiles rows tileIndex = do
        tile <- readTile tileIndex
        pure $ _appendTileToRows rows tile

      -- creates empty rows vector
      _emptyRows _row = Vector.replicate modelTileHeight Vector.empty

      -- vector of tile indexes in the tile row `tileRowIndex`
      tileIndexes tileRowIndex =
        Vector.generate tilesCountRow (\i -> i + tileRowIndex * tilesCountRow)
  print config
  -- print $ "Image width: " <> show imageWidth
  -- print $ "Image height: " <> show imageHeight
  -- print $ "Tile width: " <> show modelTileWidth
  -- print $ "Tile height: " <> show modelTileHeight
  -- print $ "Tiles count row: " <> show tilesCountRow
  -- print $ "Tiles count col: " <> show tilesCountCol
  -- print $ "Number of tiles: " <> show (tilesCountRow * tilesCountCol)
  -- print $ "Tile offsets length: " <> show (Vector.length tileOffsets)
  -- print $ "Tile byte counts length: " <> show (Vector.length tileByteCounts)

  -- read tiles one tile row at a time
  -- transform into pixel rows spanning the entire image and yield them
  forM_ [0 .. tilesCountCol - 1]
    $ \rowIndex -> do
      -- print $ "Tile indexes: " <> show (tileIndexes rowIndex)

      -- pixelRows <- lift $ Vector.foldM foldTiles (emptyRows rowIndex) (tileIndexes rowIndex)

      -- when (Vector.length pixelRows /= modelTileHeight) $
      --     fail $ "Failed to read tile row [" <> show rowIndex <> "]: expected " <> show modelTileHeight <> " rows, got " <> show (Vector.length pixelRows)

      -- when (Vector.any (\row -> Vector.length row /= imageWidth) pixelRows) $
      --     fail $ "Failed to read tile row [" <> show rowIndex <> "]: expected " <> show imageWidth <> " columns, got " <> show (Vector.length pixelRows)

      -- yieldMany pixelRows

      buffer <- lift $ tileIndexes rowIndex & Vector.mapM readTile

      -- let
      yieldMany
        [ Vector.slice (r * modelTileWidth) modelTileWidth (buffer ! i)
          | r <- [0 .. modelTileHeight - 1],
            i <- [0 .. tilesCountRow - 1]
        ]

-- print $ length res

-- print $ buffer ! 0 & Vector.slice 0 10
-- print $ buffer ! 1 & Vector.slice 0 10

-- print "---"

-- print $ res ! 0 & Vector.slice 0 10
-- print $ res ! 1 & Vector.slice 0 10

-- yieldMany res

-- _ <- Vector.generateM modelTileHeight (\i -> yield (tileRow ! i))

-- pure ()
-- (byteOrder, ifdOffset) <- lift $ decodeHeader <$> LBS.hGet h 8
-- lift $ hSeek h AbsoluteSeek $ fromIntegral ifdOffset
-- numTags <- lift $ readWord16 byteOrder h
-- config <- lift $ readConfig byteOrder h
-- readTileDataC byteOrder h config

encodeRowC :: (Monad m) => ConduitT (Vector Word16) ByteString m ()
encodeRowC =
  awaitForever $ \row -> do
    let write = runPut $ traverse_ putWord16le row
    yield $ toStrict write

convertTiffSafe :: FilePath -> IO ()
convertTiffSafe path =
  withBinaryFile path ReadMode $ \h -> do
    withSinkFile (path <> ".out") $ \sink -> do
      res <-
        runExceptT
          $ runConduit
          $ tiffChunksC h
          .| encodeRowC
          -- .| mapC Vector.length
          -- .| sumC
          .| sink
      -- .| sinkNull

      whenLeft_ res $ hPutStrLn stderr

-- whenRight_ res print

-- convertTiffSafe :: FilePath -> IO ()
-- convertTiffSafe path = do
--     res <-
--         withBinaryFile path ReadMode $ \h -> runExceptT $ do
--             (byteOrder, ifdOffset) <- ExceptT $ decodeHeader <$> LBS.hGet h 8
--             liftIO $ hSeek h AbsoluteSeek $ fromIntegral ifdOffset
--             numTags <- readWord16 byteOrder h
--             readConfig byteOrder h

--     print res

-- convertTiff :: Handle -> ExceptT String IO ()
-- convertTiff h = do
--   -- h <- liftIO $ openBinaryFile path ReadMode
--   (byteOrder, ifdOffset) <- ExceptT $ decodeHeader <$> LBS.hGet h 8

--   liftIO $ hSeek h AbsoluteSeek ifdOffset
--   _numTags <- readWord16 byteOrder h
--   config <- readConfig byteOrder h
--   res <- convertElevationData byteOrder h config

--   print res

data TileCache
  = Loaded (Vector Word16)
  | Empty (ExceptT String IO (Vector Word16))

-- convertElevationData :: ByteOrder -> Handle -> TiffConfig -> ExceptT String IO [(String, Int16)]
-- convertElevationData _byteOrder _h config =
--   let -- using zoom level of 12 in OSM tiles
--       -- the world map consists of 4096 x 4096 = 16 777 216 tiles at this zoom level
--       -- each tile is 256 x 256 pixels and is about 0.0879° degrees in size at the equator
--       -- each pixel is about 38.219 meters at the equator

--       -- tile index goes from 0 to 4095
--       -- since each tile is 256 x 256 pixels,
--       -- the global pixel index goes from 0 to 4095 * 255 = 1044225

--       -- the data is split into files each covering area of 1 degree by 1 degree

--       -- samplesPerMapTile = 10

--       -- raster -> model tiepoint pair associating a point (px, py, pz) in the raster space with a point (mLat, mLon, mz) in the model space
--       ModelTiePoint _tiePoint@(px, py, _pz, mLon, mLat, _mz) = config.modelTiePoint
--       -- size of raster pixel spacing in the model space units
--       ModelPixelScale _pixelScale@(xScale, yScale, _zScale) = config.modelPixelScale
--       -- TileHeight modelTileHeight = config.tileHeight
--       -- TileWidth modelTileWidth = config.tileWidth
--       -- ImageHeight imageHeight = config.imageHeight
--       -- ImageWidth imageWidth = config.imageWidth
--       -- TileOffsets tileOffsets = config.tileOffsets
--       -- TileByteCounts tileByteCounts = config.tileByteCounts

--       -- width of the image in tiles (rounded up to cover the whole image)
--       -- tilesCountRow = ceiling $ fromIntegral imageWidth / fromIntegral modelTileWidth
--       -- height of the image in tiles (rounded up to cover the whole image)
--       -- tilesCountCol = ceiling $ fromIntegral imageHeight / fromIntegral modelTileHeight

--       numTiles :: Double
--       numTiles = 4096.0 -- number of tiles at zoom level 12

--       -- top left corner coordinates in mercator units
--       (mxStart, myStart) =
--         toMercatorWeb $ GeoPoint (LatitudeDegrees mLat) (LongitudeDegrees mLon)

--       -- top left corner coordinates in global pixel units
--       (xStart, yStart) = (mxStart * numTiles * 256, myStart * numTiles * 256)
--    in -- therefore, this file covers global pixels starting from (ceiling xStart, ceiling yStart)

--       -- getTileOffset :: Int -> Either String Integer
--       -- getTileOffset i =
--       --   maybeToRight ("Failed to get tile offset for tile index [" <> show i <> "]")
--       --     $ tileOffsets
--       --     !? i
--       -- \$ tileOffsets !? (i + j * modelTileRowLength)

--       -- getTileByteCount :: Int -> Either String Integer
--       -- getTileByteCount i =
--       --   maybeToRight ("Failed to get tile byte count for tile [" <> show i <> "]")
--       --     $ tileByteCounts
--       --     !? i
--       -- \$ tileByteCounts !? (i + j * modelTileRowLength)

--       -- readTile :: Int -> ExceptT String IO (Vector Word16)
--       -- readTile i = do
--       --   offset <- liftEither (getTileOffset i)
--       --   count <- liftEither (getTileByteCount i)
--       --   liftIO $ hSeek h AbsoluteSeek offset
--       --   tileData <- readWord8Many (fromIntegral count) h
--       --   decodeLZW byteOrder tileData

--       -- allTiles =
--       --   Vector.generateM
--       --     (tilesCountRow * tilesCountCol)
--       --     readTile
--       -- [ (i, j)
--       -- \| i <- [0..modelTileRowLength - 1]
--       -- , j <- [0..modelTileColLength - 1]
--       -- ]

--       -- takePixel :: Int -> Word16
--       -- takePixel tiles index =
--       --   let -- pixel coords in the image
--       --       (i, j) = index `divMod` imageWidth
--       --       -- tile containing the pixel
--       --       (tx, ty) = (i `div` modelTileWidth, j `div` modelTileHeight)
--       --       -- pixel coords in the tile
--       --       (ti, tj) = (i `mod` modelTileWidth, j `mod` modelTileHeight)
--       --    in do
--       --         tile <-
--       --           maybeToRight ("Failed to get tile at index [" <> show (tx, ty) <> "]")
--       --             $ tiles
--       --             !? (tx * tilesCountRow + ty)
--       --         maybeToRight ("Failed to get pixel at index [" <> show (ti, tj) <> "] from tile [" <> show (tx, ty) <> "]")
--       --           $ tile
--       --           !? (ti * modelTileWidth + tj)
--       -- let
--       --     (i, j) = index `divMod` modelTileWidth
--       --     tileIndex = i * modelTileRowLength + j
--       --     tile = allTiles ! tileIndex
--       --     (x, y) = pi `divMod` imageWidth
--       --     pixelIndex = y * modelTileWidth + x
--       -- in tile ! pixelIndex

--       -- fullImageC :: (MonadIO m) => Vector (Vector a) -> ConduitT () Word16 (ResourceT m) ()
--       -- fullImageC tiles =
--       --     yieldMany (Vector.generate (imageWidth * imageHeight) identity)
--       --     .| mapMC (takePixel tiles)
--       -- Vector.generateM (imageWidth * imageHeight) $ \i -> do
--       --     r <- liftIO $ takePixel tiles i
--       --     yield r

--       do
--         print $ "px: " ++ show px ++ ", py: " ++ show py
--         print $ "mLon: " ++ show mLon ++ ", mLat: " ++ show mLat
--         print $ "xScale: " ++ show xScale ++ ", yScale: " ++ show yScale
--         print $ "xStart: " ++ show xStart ++ ", yStart: " ++ show yStart
--         pure []

decodeHeader :: LBS.ByteString -> Either String (ByteOrder, Integer)
decodeHeader = runGetOrError $ do
  byteOrder <- decodeByteOrder
  checkNum <- decodeWord16 byteOrder

  when (checkNum /= 42) $ fail "Invalid TIFF file"

  offset <- fromIntegral <$> decodeWord32 byteOrder

  pure (byteOrder, offset)

geoKeys :: [Word16] -> [Double] -> String -> Either String [GeoKey]
geoKeys keyEntries doubleParams asciiParams =
  let readKeyEntry :: [Word16] -> Either String (GeoKey, [Word16])
      -- ModelType
      readKeyEntry (1024 : 0 : 1 : 2 : rest) =
        Right (GTModelType Geographic, rest)
      readKeyEntry (1024 : 0 : 1 : value) =
        Left $ "ModelType not supported: " <> show value
      -- RasterType
      readKeyEntry (1025 : 0 : 1 : 1 : rest) =
        Right (GTRasterType PixelIsArea, rest)
      readKeyEntry (1025 : 0 : 1 : 2 : rest) =
        Right (GTRasterType PixelIsPoint, rest)
      readKeyEntry (1025 : 0 : 1 : value) =
        Left $ "RasterType not supported: " <> show value
      -- GeographicType
      readKeyEntry (2048 : 0 : 1 : 4326 : rest) =
        Right (GeographicType WGS_84, rest)
      readKeyEntry (2048 : 0 : 1 : value) =
        Left $ "GeographicType not supported: " <> show value
      -- GeogCitation
      readKeyEntry (2049 : 34737 : count : offset : rest) =
        Right
          ( GeogCitation $ (take (fromIntegral count) . drop (fromIntegral offset)) asciiParams,
            rest
          )
      -- GeogAngularUnits
      readKeyEntry (2054 : 0 : 1 : 9102 : rest) =
        Right (GeogAngularUnits Degree, rest)
      readKeyEntry (2054 : 0 : 1 : value : _rest) =
        Left $ "GeogAngularUnits not supported: " <> show value
      -- GeogSemiMajorAxis
      readKeyEntry (2057 : 34736 : 1 : offset : rest) =
        (,rest)
          . GeogSemiMajorAxis
          <$> maybeToRight
            "Failed to read value from GeoDoubleParams. Index out of range."
            (maybeAt (fromIntegral offset) doubleParams)
      readKeyEntry (2057 : 34736 : count : _offset : _rest) =
        Left $ "GeogSemiMajorAxis: invalid dataCount: " <> show count <> " (Should be 1)"
      -- GeogInvFlattening
      readKeyEntry (2059 : 34736 : 1 : offset : rest) =
        (,rest)
          . GeogInvFlattening
          <$> maybeToRight
            "Failed to read value from GeoDoubleParams. Index out of range."
            (maybeAt (fromIntegral offset) doubleParams)
      readKeyEntry (2059 : 34736 : count : _offset : _rest) =
        Left $ "GeogInvFlattening: invalid dataCount: " <> show count <> " (Should be 1)"
      readKeyEntry (keyId : _location : _count : _value : _rest) =
        Left $ "Unsupported GeoKey: " <> show keyId
      readKeyEntry _ =
        Left "Invalid GeoKeyDirectory entry"

      readKeys :: [Word16] -> Either String [GeoKey]
      readKeys ks =
        case readKeyEntry ks of
          Right (k, []) -> pure [k]
          Right (k, ks') -> (:) k <$> readKeys ks'
          Left err -> Left err
   in readKeys keyEntries

decodeByteOrder :: Get ByteOrder
decodeByteOrder = do
  byteOrder1 <- getWord8
  byteOrder2 <- getWord8
  case (byteOrder1, byteOrder2) of
    (0x49, 0x49) -> pure LittleEndian
    (0x4D, 0x4D) -> pure BigEndian
    _ -> fail "Failed to decode byte order"

readWord8Many :: Int -> Handle -> ExceptT String IO (Vector Word8)
readWord8Many count h =
  -- ExceptT $ runGetOrError (replicateM count getWord8) <$> LBS.hGet h count
  ExceptT $ runGetOrError (Vector.replicateM count getWord8) <$> LBS.hGet h count

decodeWord16 :: ByteOrder -> Get Word16
decodeWord16 bo =
  case bo of
    LittleEndian -> getWord16le
    BigEndian -> getWord16be

encodeWord16 :: ByteOrder -> Word16 -> Put
encodeWord16 bo w =
  case bo of
    LittleEndian -> putWord16le w
    BigEndian -> putWord16be w

readWord16 :: ByteOrder -> Handle -> ExceptT String IO Word16
readWord16 bo h =
  ExceptT $ runGetOrError (decodeWord16 bo) <$> LBS.hGet h 2

readWord16Many :: Int -> ByteOrder -> Handle -> ExceptT String IO [Word16]
readWord16Many count bo h =
  ExceptT $ runGetOrError (replicateM count (decodeWord16 bo)) <$> LBS.hGet h (2 * count)

decodeWord32 :: ByteOrder -> Get Word32
decodeWord32 bo =
  case bo of
    LittleEndian -> getWord32le
    BigEndian -> getWord32be

readWord32 :: ByteOrder -> Handle -> ExceptT String IO Word32
readWord32 bo h =
  ExceptT $ runGetOrError (decodeWord32 bo) <$> LBS.hGet h 4

readWord32Many :: Int -> ByteOrder -> Handle -> ExceptT String IO [Word32]
readWord32Many count bo h =
  ExceptT $ runGetOrError (replicateM count (decodeWord32 bo)) <$> LBS.hGet h (4 * count)

decodeDouble :: ByteOrder -> Get Double
decodeDouble bo =
  case bo of
    LittleEndian -> getDoublele
    BigEndian -> getDoublebe

readDouble :: ByteOrder -> Handle -> ExceptT String IO Double
readDouble bo h =
  ExceptT $ runGetOrError (decodeDouble bo) <$> LBS.hGet h 8

readDoubleMany :: Int -> ByteOrder -> Handle -> ExceptT String IO [Double]
readDoubleMany count bo h =
  ExceptT $ runGetOrError (replicateM count (decodeDouble bo)) <$> LBS.hGet h (8 * count)

runGetOrError :: Get a -> LBS.ByteString -> Either String a
runGetOrError get' input =
  case runGetOrFail get' input of
    Left (_, _, e) -> Left e
    Right (_, _, a) -> Right a
