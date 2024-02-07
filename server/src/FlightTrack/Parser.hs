module FlightTrack.Parser where

import Data.Time
import FlightTrack (FlightTrack (..))
import Geo (Elevation (..), Latitude (..), Longitude (..), ddmTodd)
import Relude
import Text.Parsec
  ( Parsec,
    alphaNum,
    char,
    choice,
    count,
    digit,
    eof,
    many1,
    noneOf,
    sepEndBy1,
    string,
    try,
  )
import TrackPoint (FixValidity (..), TrackPoint (TrackPoint))

data FlightInfo
  = FlightDate UTCTime
  | CompId Text
  | Fix TrackPoint
  | UnknownRecord
  deriving stock (Show, Eq)

buildFlightTrack :: String -> [FlightInfo] -> Either String FlightTrack
buildFlightTrack name flightInfoItems = do
  (fd, items) <- flightDate flightInfoItems
  (ci, items') <- compId items
  tps <- trackPoints items'

  pure $ FlightTrack fd ci tps
  where
    fixTrackPoint (Fix tp) = Just tp
    fixTrackPoint _ = Nothing

    trackPoints :: [FlightInfo] -> Either String (NonEmpty TrackPoint)
    trackPoints = maybeToRight (name <> ": No track points") . nonEmpty . mapMaybe fixTrackPoint

    flightDate :: [FlightInfo] -> Either String (UTCTime, [FlightInfo])
    flightDate infoItems =
      case infoItems of
        (FlightDate date) : rest -> Right (date, rest)
        _ : rest -> flightDate rest
        [] -> Left (name <> ": Missing or invalid location for Flight Date info")

    compId :: [FlightInfo] -> Either String (Text, [FlightInfo])
    compId infoItems =
      case infoItems of
        (CompId cid) : rest -> Right (cid, rest)
        _ : rest -> compId rest
        [] -> Left (name <> ": Missing or invalid location for Competition ID info")

-- combineFlightInfo :: FlightInfo -> FlightInfo -> [FlightInfo] -> Either String FlightTrack
-- combineFlightInfo (FlightDate flightDateInfo) (CompId compIdInfo) pointsInfo =
--    FlightTrack flightDateInfo compIdInfo <$> trackPoints pointsInfo

-- combineFlightInfo (FlightDate _) _ _ = Left "Missing or invalid location for Competition ID info"
-- combineFlightInfo _ _ _ = Left "Missing or invalid location for Flight Date info"

trackDateIdentifier :: Parsec Text () ()
trackDateIdentifier =
  string "HFDTE" *> optional (string "DATE:") $> ()

trackDateParser :: Parsec Text () FlightInfo
trackDateParser = do
  -- string "DTE"
  -- optional $ string "DATE:"

  day <- readEither <$> count 2 digit
  month <- readEither <$> count 2 digit
  year <- fmap (+ 2000) . readEither <$> count 2 digit

  void $ many (noneOf "\n")

  case fromGregorian <$> year <*> month <*> day of
    Right x -> pure $ FlightDate $ UTCTime x 0
    Left e -> fail $ "Failed to parse flight track date:" <> show e

compIdIdentifier :: Parsec Text () ()
compIdIdentifier =
  string "HFCIDCOMPETITIONID:" $> ()

compIdParser :: Parsec Text () FlightInfo
compIdParser =
  CompId . toText <$> many1 alphaNum

readInt :: String -> Either Text Int
readInt = readEither

trackPointIdentifier :: Parsec Text () ()
trackPointIdentifier =
  string "B" $> ()

trackPointTimeParser :: Parsec Text () DiffTime
trackPointTimeParser = do
  hours <- readEither <$> count 2 digit
  minutes <- readEither <$> count 2 digit
  seconds <- readEither <$> count 2 digit

  case (\h m s -> secondsToDiffTime $ 3600 * h + 60 * m + s) <$> hours <*> minutes <*> seconds of
    Right x -> pure x
    Left e -> fail $ "Failed to parse track point time: " <> show e

trackPointLatitudeParser :: Parsec Text () Latitude
trackPointLatitudeParser = do
  deg <- readEither <$> count 2 digit
  minutes <- count 2 digit
  decMin <- count 3 digit
  adjustForHemisphereFn <-
    choice
      [ id <$ char 'N',
        negate <$ char 'S' -- need to negate the value for southern hemisphere
      ]

  case ddmTodd <$> deg <*> readEither (minutes ++ "." ++ decMin) of
    Right x -> pure $ LatitudeDegrees $ adjustForHemisphereFn x
    Left e -> fail $ "Failed to parse latitude: " ++ toString e

trackPointLongitudeParser :: Parsec Text () Longitude
trackPointLongitudeParser = do
  deg <- readEither <$> count 3 digit
  minutes <- count 2 digit
  decMin <- count 3 digit
  adjustForHemisphereFn <-
    choice
      [ id <$ char 'E',
        negate <$ char 'W' -- need to negate the value for western hemisphere
      ]

  case ddmTodd <$> deg <*> readEither (minutes ++ "." ++ decMin) of
    Right x -> pure $ LongitudeDegrees $ adjustForHemisphereFn x
    Left e -> fail $ "Failed to parse longitude: " ++ toString e

trackPointAltitudeParser :: Parsec Text () Elevation
trackPointAltitudeParser = do
  alt <- readEither <$> count 5 digit
  case alt of
    Right x -> pure $ ElevationMeters x
    Left e -> fail $ "Failed to parse altitude: " ++ toString e

fixValidityParser :: Parsec Text () FixValidity
fixValidityParser =
  (Gps3D <$ char 'A') <|> (Baro2D <$ char 'V')

trackPointParser :: Parsec Text () TrackPoint
trackPointParser =
  TrackPoint
    <$> trackPointTimeParser
    <*> trackPointLatitudeParser
    <*> trackPointLongitudeParser
    <*> fixValidityParser
    <*> trackPointAltitudeParser
    <*> trackPointAltitudeParser
    <* many (noneOf ['\n', '\r'])

unknownRecordParser :: Parsec Text () FlightInfo
unknownRecordParser =
  UnknownRecord
    -- <$ oneOf ['A', 'B', 'C', 'D', 'E', 'F', 'G', 'H', 'I', 'J', 'K', 'L', 'M', 'N', 'O']
    <$ many (noneOf ['\n', '\r'])

flightInfoParser :: Parsec Text () FlightInfo
flightInfoParser =
  choice
    [ try trackDateIdentifier *> trackDateParser,
      try compIdIdentifier *> compIdParser,
      try trackPointIdentifier *> (Fix <$> trackPointParser),
      unknownRecordParser
    ]

flightInfoParserAll :: Parsec Text () [FlightInfo]
flightInfoParserAll =
  sepEndBy1
    ( choice
        [ try trackDateIdentifier *> trackDateParser,
          try compIdIdentifier *> compIdParser,
          try trackPointIdentifier *> (Fix <$> trackPointParser),
          unknownRecordParser
        ]
        -- <|> try
    )
    -- ( choice
    --     [ char 'H' *>
    --         ( (char 'F' *> ( trackDateParser <|> compIdParser <|> unknownRecordParser ))
    --         <|> unknownRecordParser
    --         )
    --     , char 'B' *> (Fix <$> trackPointParser)
    --     , unknownRecordParser
    --     ]
    -- )
    (many1 (char '\n' <|> char '\r'))
    <* eof
