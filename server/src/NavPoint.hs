module NavPoint where

import Data.Aeson qualified as Aeson
import Generics.SOP qualified as SOP
import Geo (Direction (..), Distance (..), Elevation (..), GeoPosition (..), Latitude (..), Longitude (..), ddmTodd)
import Language.Haskell.To.Elm (HasElmDecoder, HasElmEncoder, HasElmType)
import Magic.ElmDeriving (ElmType)
import Relude
import Text.Parsec (Parsec, between, choice, count, digit, eof, letter, many1, noneOf, oneOf, option, optionMaybe, sepEndBy1, string)
import Text.Parsec.Char (char)

data WaypointStyle
  = Unknown
  | Waypoint
  | AirfieldGrass
  | Outlanding
  | AirfieldGliding
  | AirfieldSolid
  | MountainPass
  | MountainTop
  | TransmitterMast
  | VOR
  | NDB
  | CoolingTower
  | Dam
  | Tunnel
  | Bridge
  | PowerPlant
  | Castle
  | Intersection
  deriving stock (Show, Read, Eq, Generic)
  deriving anyclass (SOP.Generic, SOP.HasDatatypeInfo, Aeson.ToJSON, Aeson.FromJSON)
  deriving
    (HasElmType, HasElmEncoder Aeson.Value, HasElmDecoder Aeson.Value)
    via ElmType "Api.Types.WaypointStyle" WaypointStyle

data NavPoint = NavPoint
  { name :: Text,
    code :: Text,
    country :: Maybe Text,
    -- , position :: Position WGS84
    lat :: Latitude,
    lon :: Longitude,
    elev :: Elevation,
    style :: WaypointStyle,
    rwdir :: Maybe Direction,
    rwlen :: Maybe Distance,
    freq :: Maybe Text,
    desc :: Text
  }
  deriving stock (Show, Read, Eq, Generic)
  deriving anyclass (SOP.Generic, SOP.HasDatatypeInfo, Aeson.ToJSON, Aeson.FromJSON)
  deriving
    (HasElmType, HasElmEncoder Aeson.Value, HasElmDecoder Aeson.Value)
    via ElmType "Api.Types.NavPoint" NavPoint

instance GeoPosition NavPoint where
  latitude = lat
  longitude = lon

-- navPointPositionParser :: Parsec String () (Position WGS84)
-- navPointPositionParser =
--     wgs84Position
--     <$> navPointLatParser
--     <* char ','
--     <*> navPointLonParser
--     <* char ','
--     <*> navPointElevationParser

--     where
--         wgs84Position lat lon elev = latLongHeightPos lat lon (metres elev) WGS84

navPointLinesParser :: Parsec Text () [NavPoint]
navPointLinesParser =
  sepEndBy1 navPointParser (many1 $ choice [char '\n', char '\r', char ' ']) <* eof

navPointParser :: Parsec Text () NavPoint
navPointParser =
  NavPoint
    <$> navPointNameParser
    <* char ','
    <*> navPointCodeParser
    <* char ','
    <*> navPointCountryParser
    <* char ','
    <*> navPointLatParser
    <* char ','
    <*> navPointLonParser
    <* char ','
    <*> navPointElevationParser
    <* char ','
    <*> navPointStyleParser
    <* char ','
    <*> optionMaybe navPointRwdirParser
    <* char ','
    <*> optionMaybe navPointRwlenParser
    <* char ','
    <*> optionMaybe navPointFreqParser
    <* char ','
    <*> (toText <$> inQuotations (many1 $ noneOf ['"']))

navPointNameParser :: Parsec Text () Text
navPointNameParser =
  toText <$> inQuotations (many1 $ noneOf ['"'])

navPointCodeParser :: Parsec Text () Text
navPointCodeParser =
  toText
    <$> choice
      [ inQuotations (many1 $ noneOf ['"']),
        many1 $ noneOf [',']
      ]

navPointCountryParser :: Parsec Text () (Maybe Text)
navPointCountryParser =
  optionMaybe $ toText <$> many1 letter

navPointStyleParser :: Parsec Text () WaypointStyle
navPointStyleParser =
  (<>) <$> digitAsString <*> option "" digitAsString >>= matchIdParser
  where
    digitAsString :: Parsec Text () String
    digitAsString = one <$> digit

    matchIdParser :: String -> Parsec Text () WaypointStyle
    matchIdParser "0" = pure Unknown
    matchIdParser "1" = pure Waypoint
    matchIdParser "2" = pure AirfieldGrass
    matchIdParser "3" = pure Outlanding
    matchIdParser "4" = pure AirfieldGliding
    matchIdParser "5" = pure AirfieldSolid
    matchIdParser "6" = pure MountainPass
    matchIdParser "7" = pure MountainTop
    matchIdParser "8" = pure TransmitterMast
    matchIdParser "9" = pure VOR
    matchIdParser "10" = pure NDB
    matchIdParser "11" = pure CoolingTower
    matchIdParser "12" = pure Dam
    matchIdParser "13" = pure Tunnel
    matchIdParser "14" = pure Bridge
    matchIdParser "15" = pure PowerPlant
    matchIdParser "16" = pure Castle
    matchIdParser "17" = pure Intersection
    matchIdParser npid = fail $ "Failed to parse waypoint style. Unknown style: " <> npid

navPointLatParser :: Parsec Text () Latitude
navPointLatParser = do
  deg <- readEither <$> count 2 digit
  minutes <- count 2 digit
  void $ char '.'
  decMin <- count 3 digit
  adjustForHemisphereFn <-
    choice
      [ id <$ char 'N',
        negate <$ char 'S' -- need to negate the value for southern hemisphere
      ]

  case ddmTodd <$> deg <*> readEither (minutes ++ "." ++ decMin) of
    Right x -> pure $ LatitudeDegrees $ adjustForHemisphereFn x
    Left e -> fail $ "Failed to parse latitude: " ++ toString e

navPointLonParser :: Parsec Text () Longitude
navPointLonParser = do
  deg <- readEither <$> count 3 digit
  minutes <- count 2 digit
  void $ char '.'
  decMin <- count 3 digit
  adjustForHemisphereFn <-
    choice
      [ id <$ char 'E',
        negate <$ char 'W' -- need to negate the value for western hemisphere
      ]

  case ddmTodd <$> deg <*> readEither (minutes ++ "." ++ decMin) of
    Right x -> pure $ LongitudeDegrees $ adjustForHemisphereFn x
    Left e -> fail $ "Failed to parse longitude: " ++ toString e

navPointElevationParser :: Parsec Text () Elevation
navPointElevationParser = do
  elev <- doubleParser
  unitConversionFn <-
    choice
      [ id <$ char 'm', -- meters
        (/ 3.2808) <$ string "ft" -- feet
      ]

  pure $ ElevationMeters $ unitConversionFn elev

navPointRwdirParser :: Parsec Text () Direction
navPointRwdirParser = do
  dir <- readEither <$> count 1 (oneOf ['0', '1', '2', '3']) <> count 2 digit

  case dir of
    Right x -> pure $ DirectionDegrees x
    Left e -> fail $ "Failed to parse rwdir: " ++ toString e

navPointRwlenParser :: Parsec Text () Distance
navPointRwlenParser = do
  len <- doubleParser
  unit <-
    choice
      [ char 'm' >> option id ml, -- meters 'm' or statute miles 'ml'
        (* 1852) <$ string "nm" -- nautical miles
      ]

  pure $ DistanceMeters $ unit len
  where
    ml :: Parsec Text u (Double -> Double)
    ml = (* 1609.344) <$ string "l" -- statute miles

navPointFreqParser :: Parsec Text () Text
navPointFreqParser =
  toText <$> choice [inQuotations freq, freq]
  where
    freq :: Parsec Text () String
    freq =
      count 1 (char '1')
        <> count 1 (oneOf ['1', '2', '3'])
        <> count 1 digit
        <> count 1 (char '.')
        <> count 2 digit
        <> count 1 (oneOf ['0', '5'])

intParser :: Parsec Text () Int
intParser = do
  res <- readEither <$> many1 digit
  case res of
    Right i -> pure i
    Left e -> fail $ "Failed to parse int: " ++ toString e

fractionParser :: Parsec Text u String
fractionParser = count 1 (char '.') <> many1 digit

doubleParser :: Parsec Text () Double
doubleParser = do
  sign <- optionMaybe $ string "-"
  integral <- Just <$> many1 digit
  fractional <- optionMaybe fractionParser

  case readEither $ fold $ sign <> integral <> fractional of
    Right x -> pure x
    Left e -> fail $ "Failed to parse double: " ++ toString e

inQuotations :: Parsec Text () a -> Parsec Text () a
inQuotations =
  between (char '"') (char '"')
