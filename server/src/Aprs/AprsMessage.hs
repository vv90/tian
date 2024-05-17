module Aprs.AprsMessage where

import Aprs.AprsSymbol (AprsSymbol, matchSymbol)
import Aprs.GlidernetId (GlidernetIdInfo, glidernetIdParser)
import Data.Aeson qualified as Aeson
import Data.Time (secondsToDiffTime)
import Generics.SOP qualified as SOP
import Geo
  ( AngularSpeed (DegreesPerSecond),
    Direction (..),
    Elevation (..),
    GeoPosition (..),
    GeoPosition3d (..),
    Latitude (LatitudeDegrees),
    Longitude (LongitudeDegrees),
    MovingGeoPosition (..),
    RecordedGeoPosition (..),
    Speed (..),
    ddmTodd,
    elevationFeet,
  )
import Language.Haskell.To.Elm (HasElmDecoder, HasElmEncoder, HasElmType)
import Magic.ElmDeriving (ElmType)
import Relude
import Text.Parsec
  ( Parsec,
    alphaNum,
    between,
    char,
    choice,
    letter,
    many1,
    noneOf,
    oneOf,
    option,
    sepEndBy,
    spaces,
    string,
  )
import Text.Parsec.Char (digit)
import Text.Parsec.Combinator (count)

newtype DeviceId = DeviceId Text
  deriving stock (Show, Eq, Generic)
  deriving anyclass (Hashable, SOP.Generic, SOP.HasDatatypeInfo, Aeson.ToJSON, Aeson.FromJSON)
  deriving
    (HasElmType, HasElmEncoder Aeson.Value, HasElmDecoder Aeson.Value)
    via ElmType "Api.Types.DeviceId" DeviceId

getDeviceId :: DeviceId -> Text
getDeviceId (DeviceId x) = x

data AprsMessage = AprsMessage
  { source :: DeviceId,
    symbol :: AprsSymbol,
    timeSeconds :: Int,
    lat :: Latitude,
    lon :: Longitude,
    heading :: Direction,
    speed :: Speed,
    elev :: Elevation,
    glidernetId :: Maybe GlidernetIdInfo,
    verticalSpeed :: Maybe Speed,
    rotation :: Maybe AngularSpeed
  }
  deriving stock (Show, Eq)

instance GeoPosition AprsMessage where
  latitude x = x.lat
  longitude x = x.lon

instance GeoPosition3d AprsMessage where
  altitude x = x.elev

instance RecordedGeoPosition AprsMessage where
  time = secondsToDiffTime . fromIntegral . timeSeconds

instance MovingGeoPosition AprsMessage where
  speed x = x.speed
  heading x = x.heading

aprsLatParser :: Parsec ByteString () Latitude
aprsLatParser = do
  degrees <- readEither <$> count 2 digit
  minutes <- count 2 digit
  void $ char '.'
  decMin <- count 2 digit

  adjustForHemisphereFn <-
    choice
      [ id <$ char 'N',
        negate <$ char 'S' -- need to negate the value for southern hemisphere
      ]

  case ddmTodd <$> degrees <*> readEither (minutes ++ "." ++ decMin) of
    Right x -> pure $ LatitudeDegrees $ adjustForHemisphereFn x
    Left e -> fail $ "Failed to parse latitude: " ++ toString e

aprsLonParser :: Parsec ByteString () Longitude
aprsLonParser = do
  deg <- readEither <$> count 3 digit
  minutes <- count 2 digit
  void $ char '.'
  decMin <- count 2 digit

  adjustForHemisphereFn <-
    choice
      [ id <$ char 'E',
        negate <$ char 'W' -- need to negate the value for western hemisphere
      ]

  case ddmTodd <$> deg <*> readEither (minutes ++ "." ++ decMin) of
    Right x -> pure $ LongitudeDegrees $ adjustForHemisphereFn x
    Left e -> fail $ "Failed to parse longitude: " ++ toString e

aprsTimeParser :: Parsec ByteString () Int
aprsTimeParser = do
  hrs <- readEither <$> count 2 digit
  mins <- readEither <$> count 2 digit
  secs <- readEither <$> count 2 digit

  case (\h m s -> h * 3600 + m * 60 + s) <$> hrs <*> mins <*> secs of
    Right x -> pure x
    Left e -> fail $ "Failed to parse time: " ++ toString e

aprsAltParser :: Parsec ByteString () Elevation
aprsAltParser = do
  void $ string "A="
  alt <- readEither <$> count 6 digit
  case alt of
    Right x -> pure $ elevationFeet x
    Left e -> fail $ "Failed to parse altitude: " ++ toString e

aprsDirectionParser :: Parsec ByteString () Direction
aprsDirectionParser = do
  dir <- readEither <$> count 3 digit
  case dir of
    Right x -> pure $ DirectionDegrees x
    Left e -> fail $ "Failed to parse direction: " ++ toString e

aprsSpeedParser :: Parsec ByteString () Speed
aprsSpeedParser = do
  spd <- readEither @Int <$> count 3 digit
  case spd of
    Right x -> pure $ SpeedMetersPerSecond $ (fromIntegral x * 1852) / 3600 -- convert knots to m/s
    Left e -> fail $ "Failed to parse speed: " ++ toString e

aprsSourceParser :: Parsec ByteString () Text
aprsSourceParser =
  toText <$> many1 alphaNum

primarySymbolParser :: Parsec ByteString () Char
primarySymbolParser = oneOf ['/', '\\']

secondarySymbolParser :: Parsec ByteString () Char
secondarySymbolParser =
  oneOf ['!', '"', '#', '$', '%', '&', '\'', '(', ')', '*', '+', ',', '-', '.', '/', '0', '1', '2', '3', '4', '5', '6', '7', '8', '9', ':', ';', '<', '=', '>', '?', '@', 'A', 'B', 'C', 'D', 'E', 'F', 'G', 'H', 'I', 'J', 'K', 'L', 'M', 'N', 'O', 'P', 'Q', 'R', 'S', 'T', 'U', 'V', 'W', 'X', 'Y', 'Z', '[', '\\', ']', '^', '_', '`', 'a', 'b', 'c', 'd', 'e', 'f', 'g', 'h', 'i', 'j', 'k', 'l', 'm', 'n', 'o', 'p', 'q', 'r', 's', 't', 'u', 'v', 'w', 'x', 'y', 'z', '{', '|', '}', '~']

aprsInfoParser :: Parsec ByteString () (Direction, Speed, Elevation)
aprsInfoParser =
  (,,)
    <$> aprsDirectionParser
    <* char '/'
    <*> aprsSpeedParser
    <* char '/'
    <*> aprsAltParser

aprsPrecisionEnhancementParser :: Parsec ByteString () (Int, Int)
aprsPrecisionEnhancementParser =
  let digitParser :: Parsec ByteString () Int
      digitParser = do
        d <- readEither @Int . one <$> digit

        either (\e -> fail $ "Failed to parse digit: " ++ toString e) pure d
   in do
        between (char '!') (char '!') ((,) <$ char 'W' <*> digitParser <*> digitParser)

data AprsAdditionalInfo
  = GlidernetId GlidernetIdInfo
  | VerticalSpeed Speed -- fpm
  | Rotation AngularSpeed -- rot
  | SignalToNoise Double -- dB
  | ErrorRate Double -- e
  | FrequencyOffset Double -- kHz
  | PowerRatio Double -- dBm
  | PrecisionDigits (Int, Int)
  | OtherInfo Text

aprsAdditionalInfoParser :: Parsec ByteString () AprsAdditionalInfo
aprsAdditionalInfoParser =
  let numericValueParser :: Parsec ByteString () AprsAdditionalInfo
      numericValueParser = do
        adjustForSign <-
          option id
            $ choice
              [ id <$ char '+',
                negate <$ char '-'
              ]

        value <- readEither @Double <$> many1 (digit <|> char '.')
        unit <- many1 letter

        case (adjustForSign <$> value, unit) of
          (Right x, "fpm") -> pure $ VerticalSpeed $ SpeedMetersPerSecond x
          (Right x, "rot") -> pure $ Rotation $ DegreesPerSecond x
          (Right x, "dB") -> pure $ SignalToNoise x
          (Right x, "e") -> pure $ ErrorRate x
          (Right x, "kHz") -> pure $ FrequencyOffset x
          (Right x, "dBm") -> pure $ PowerRatio x
          (Right _, _) -> fail $ "Unknown unit: " ++ unit
          (Left e, _) -> fail $ "Failed to parse numeric value: " ++ toString e
   in do
        PrecisionDigits <$> aprsPrecisionEnhancementParser
        <|> (GlidernetId <$> glidernetIdParser)
        <|> numericValueParser
        <|> (OtherInfo . toText <$> many1 (noneOf [' ', '\n']))

applyAdditionalInfo :: AprsMessage -> AprsAdditionalInfo -> AprsMessage
applyAdditionalInfo msg (GlidernetId gnId) = msg {glidernetId = Just gnId}
applyAdditionalInfo msg (VerticalSpeed vs) = msg {verticalSpeed = Just vs}
applyAdditionalInfo msg (Rotation rot) = msg {rotation = Just rot}
applyAdditionalInfo msg (PrecisionDigits (latDigit, lonDigit)) =
  -- precision digits provide the third digit after the decimal point (in decimal minutes)
  -- we need to convert it to decimal degrees and add it to the existing value
  -- divide by 60 (minutes in a degree) * 1000 (third decimal place) = 60000
  let (LatitudeDegrees lat, LongitudeDegrees lon) = (msg.lat, msg.lon)
      adjustedLat :: Double
      adjustedLat =
        if lat > 0
          then lat + (fromIntegral latDigit / 60000)
          else lat - (fromIntegral latDigit / 60000)

      adjustedLon :: Double
      adjustedLon =
        if lon > 0
          then lon + (fromIntegral lonDigit / 60000)
          else lon - (fromIntegral lonDigit / 60000)
   in msg
        { lat = LatitudeDegrees adjustedLat,
          lon = LongitudeDegrees adjustedLon
        }
applyAdditionalInfo msg _ = msg

aprsMessageParser :: Parsec ByteString () AprsMessage
aprsMessageParser = do
  deviceId <- DeviceId <$> aprsSourceParser
  void $ char '>' <* many1 (noneOf [':']) <* char ':' <* char '/'
  messageTime <- aprsTimeParser
  void $ char 'h'
  messageLat <- aprsLatParser
  primarySymbol <- primarySymbolParser
  messageLon <- aprsLonParser
  secondarySymbol <- secondarySymbolParser
  (heading', speed', alt) <- aprsInfoParser
  void spaces

  additionalValues <- aprsAdditionalInfoParser `sepEndBy` spaces

  pure
    $ foldl'
      applyAdditionalInfo
      ( AprsMessage
          { source = deviceId,
            symbol = matchSymbol (primarySymbol, secondarySymbol),
            timeSeconds = messageTime,
            lat = messageLat,
            lon = messageLon,
            heading = heading',
            speed = speed',
            elev = alt,
            glidernetId = Nothing,
            verticalSpeed = Nothing,
            rotation = Nothing
          }
      )
      additionalValues
