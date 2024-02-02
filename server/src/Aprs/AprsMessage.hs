module Aprs.AprsMessage where

import Data.Time (DiffTime, secondsToDiffTime)
import Geo (Elevation (..), GeoPosition (..), GeoPosition3d (..), Latitude (LatitudeDegrees), Longitude (LongitudeDegrees), RecordedGeoPosition (..), ddmTodd, elevationFeet)
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

-- should match one of the following:
-- /
-- \\ -- two backslashes (without escaping)
separatorParser :: Parsec ByteString () ()
separatorParser =
  void $ choice [char '/', char '\\']

aprsLatParser :: Parsec ByteString () Latitude
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

aprsLonParser :: Parsec ByteString () Longitude
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

aprsTimeParser :: Parsec ByteString () DiffTime
aprsTimeParser = do
  hrs <- readEither <$> count 2 digit
  mins <- readEither <$> count 2 digit
  secs <- readEither <$> count 2 digit

  case (\h m s -> h * 3600 + m * 60 + s) <$> hrs <*> mins <*> secs of
    Right x -> pure $ secondsToDiffTime x
    Left e -> fail $ "Failed to parse time: " ++ toString e

aprsAltParser :: Parsec ByteString () Elevation
aprsAltParser = do
  void $ string "A="
  alt <- readEither <$> count 6 digit
  case alt of
    Right x -> pure $ elevationFeet x
    Left e -> fail $ "Failed to parse altitude: " ++ toString e

aprsSourceParser :: Parsec ByteString () Text
aprsSourceParser =
  toText <$> many1 alphaNum

aprsMessageParser :: Parsec ByteString () AprsMessage
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
    <* choice [string "/", string "\\\\"]
    <*> aprsLonParser
    <* choice [char '\'', char '^', char 'n', char 'g', char 'O', char 'z', char 'X']
    <* count 3 digit
    <* char '/'
    <* count 3 digit
    <* char '/'
    <*> aprsAltParser

-- FLRDDE626>APRS,qAS,EGHL:/074548h5111.32N/00102.04W'086/007/A=000607 id0ADDE626 -019fpm +0.0rot 5.5dB 3e -4.3kHz
-- The APRS symbols are the ones used to separate the altitude of the longitude (for example / on the above lines) and the symbol used to separate the longitude from the course/speed (for example ' on the above lies)

-- "/z",  //  0 = ?
-- "/'",  //  1 = (moto-)glider    (most frequent)
-- "/'",  //  2 = tow plane        (often)
-- "/X",  //  3 = helicopter       (often)
-- "/g" , //  4 = parachute        (rare but seen - often mixed with drop plane)
-- "\\^", //  5 = drop plane       (seen)
-- "/g" , //  6 = hang-glider      (rare but seen)
-- "/g" , //  7 = para-glider      (rare but seen)
-- "\\^", //  8 = powered aircraft (often)
-- "/^",  //  9 = jet aircraft     (rare but seen)
-- "/z",  //  A = UFO              (people set for fun)
-- "/O",  //  B = balloon          (seen once)
-- "/O",  //  C = airship          (seen once)
-- "/'",  //  D = UAV              (drones, can become very common)
-- "/z",  //  E = ground support   (ground vehicles at airfields)
-- "\\n"  //  F = static object    (ground relay ?)
