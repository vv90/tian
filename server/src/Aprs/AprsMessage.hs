
module Aprs.AprsMessage where

import Relude
import Data.Time (DiffTime, secondsToDiffTime)
import Geo (Latitude (LatitudeDegrees), Longitude (LongitudeDegrees), Elevation (ElevationMeters), GeoPosition (..), ddmTodd, GeoPosition3d (..), RecordedGeoPosition (..))
import Text.Parsec (Parsec)
import Text.Parsec.Char (digit)
import Text.Parsec.Combinator (count)
import Text.Parsec (char)
import Text.Parsec (choice)
import Text.Parsec (many1)
import Text.Parsec (alphaNum)
import Text.Parsec (noneOf, string)

data AprsMessage = AprsMessage
    { source :: Text
    , time :: DiffTime
    , lat :: Latitude
    , lon :: Longitude
    , alt :: Elevation
    }
    deriving (Show, Eq)

instance GeoPosition AprsMessage where
    latitude x = x.lat
    longitude x = x.lon

instance GeoPosition3d AprsMessage where
    altitude x = x.alt

instance RecordedGeoPosition AprsMessage where
    time x = x.time

aprsLatParser :: Parsec Text () Latitude
aprsLatParser = do
    deg <- readEither <$> count 2 digit
    min <- readEither <$> count 2 digit
    char '.'
    decMin <- readEither <$> count 2 digit
    adjustForHemisphereFn <-
        choice
            [ id <$ char 'N'
            , negate <$ char 'S' -- need to negate the value for southern hemisphere
            ]

    case ddmTodd <$> deg <*> min <*> decMin of
        Right x -> pure $ LatitudeDegrees $ adjustForHemisphereFn x
        Left e -> fail $ "Failed to parse latitude: " ++ toString e

aprsLonParser :: Parsec Text () Longitude
aprsLonParser = do
    deg <- readEither <$> count 3 digit
    min <- readEither <$> count 2 digit
    char '.'
    decMin <- readEither <$> count 2 digit
    adjustForHemisphereFn <-
        choice
            [ id <$ char 'E'
            , negate <$ char 'W' -- need to negate the value for western hemisphere
            ]

    case ddmTodd <$> deg <*> min <*> decMin of
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
    string "A="
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