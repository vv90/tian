module Aprs.AprsMessage where

import Data.Time (DiffTime, secondsToDiffTime)
import Geo (Elevation (ElevationMeters), GeoPosition (..), GeoPosition3d (..), Latitude (LatitudeDegrees), Longitude (LongitudeDegrees), RecordedGeoPosition (..), ddmTodd)
import Relude
import Text.Parsec
  ( Parsec,
    alphaNum,
    char,
    choice,
    many1,
    noneOf,
    string,
  )
import Text.Parsec.Char (digit)
import Text.Parsec.Combinator (count)

data AprsMessage = AprsMessage
  { source :: Text,
    time :: DiffTime,
    lat :: Latitude,
    lon :: Longitude,
    alt :: Elevation
  }
  deriving stock (Show, Eq)

instance GeoPosition AprsMessage where
  latitude x = x.lat
  longitude x = x.lon

instance GeoPosition3d AprsMessage where
  altitude x = x.alt

instance RecordedGeoPosition AprsMessage where
  time x = x.time

aprsLatParser :: Parsec Text () Latitude
aprsLatParser = do
  degrees <- readEither <$> count 2 digit
  minutes <- readEither <$> count 2 digit
  void $ char '.'
  decMin <- readEither <$> count 2 digit
  adjustForHemisphereFn <-
    choice
      [ id <$ char 'N',
        negate <$ char 'S' -- need to negate the value for southern hemisphere
      ]

  case ddmTodd <$> degrees <*> minutes <*> decMin of
    Right x -> pure $ LatitudeDegrees $ adjustForHemisphereFn x
    Left e -> fail $ "Failed to parse latitude: " ++ toString e

aprsLonParser :: Parsec Text () Longitude
aprsLonParser = do
  deg <- readEither <$> count 3 digit
  minutes <- readEither <$> count 2 digit
  void $ char '.'
  decMin <- readEither <$> count 2 digit
  adjustForHemisphereFn <-
    choice
      [ id <$ char 'E',
        negate <$ char 'W' -- need to negate the value for western hemisphere
      ]

  case ddmTodd <$> deg <*> minutes <*> decMin of
    Right x -> pure $ LongitudeDegrees $ adjustForHemisphereFn x
    Left e -> fail $ "Failed to parse longitude: " ++ toString e

aprsTimeParser :: Parsec Text () DiffTime
aprsTimeParser = do
  hrs <- readEither <$> count 2 digit
  mins <- readEither <$> count 2 digit
  secs <- readEither <$> count 2 digit

  case (\h m s -> h * 3600 + m * 60 + s) <$> hrs <*> mins <*> secs of
    Right x -> pure $ secondsToDiffTime x
    Left e -> fail $ "Failed to parse time: " ++ toString e

aprsAltParser :: Parsec Text () Elevation
aprsAltParser = do
  void $ string "A="
  alt <- readEither <$> count 6 digit
  case alt of
    Right x -> pure $ ElevationMeters x
    Left e -> fail $ "Failed to parse altitude: " ++ toString e

aprsSourceParser :: Parsec Text () Text
aprsSourceParser =
  toText <$> many1 alphaNum

aprsMessageParser :: Parsec Text () AprsMessage
aprsMessageParser =
  AprsMessage
    <$> aprsSourceParser
    <* char '>'
    <* many1 (noneOf [':'])
    <* char ':'
    <* char '/'
    <*> aprsTimeParser
    <* char 'h'
    <*> aprsLatParser
    <* char '/'
    <*> aprsLonParser
    <* char '\''
    <* count 3 digit
    <* char '/'
    <* count 3 digit
    <* char '/'
    <*> aprsAltParser
