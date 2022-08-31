

module NavPoint
    where

import Relude
import Data.Aeson (ToJSON, FromJSON)
import GHC.Generics ( Generic )
import Data.Int (Int32)
import Data.Text (Text, pack, unpack)
import Text.Parsec (Parsec, oneOf, digit, many1, count, between, alphaNum, choice, string, letter, optionMaybe, option, noneOf, sepEndBy1)
import Text.Parsec.Char (char, digit)
import Data.Geo.Jord.Angle (Angle, decimalDegrees)
import Data.Geo.Jord.Length (Length, metres)
import Data.Geo.Jord.Geodetic (Position, latLongHeightPos)
import Data.Geo.Jord.Models (WGS84 (WGS84))
import Data.Char (digitToInt)
import Text.Parsec.Pos (updatePosString, updatePosChar)
import Text.Parsec.Prim (tokenPrim, token, tokens, (<?>))



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
    deriving (Show, Eq)

newtype Elevation = Elevation Length
    deriving (Show)

data NavPoint = NavPoint
    { name :: Text
    , code :: Text
    , country :: Maybe Text
    , position :: Position WGS84
    , style :: WaypointStyle
    , rwdir :: Maybe Angle
    , rwlen :: Maybe Length
    , freq :: Maybe Text
    , desc :: Text
    } deriving (Show, Eq)

navPointPositionParser :: Parsec String () (Position WGS84)
navPointPositionParser =
    wgs84Position
    <$> navPointLatParser
    <* char ','
    <*> navPointLonParser
    <* char ','
    <*> navPointElevationParser

    where
        wgs84Position lat lon elev = latLongHeightPos lat lon (metres elev) WGS84

navPointLinesParser :: Parsec String () [NavPoint]
navPointLinesParser = 
    sepEndBy1 navPointParser (char '\n')

navPointParser :: Parsec String () NavPoint
navPointParser =
    NavPoint
    <$> navPointNameParser
    <* char ','
    <*> navPointCodeParser
    <* char ','
    <*> navPointCountryParser
    <* char ','
    <*> navPointPositionParser
    <* char ','
    <*> navPointStyleParser
    <* char ','
    <*> optionMaybe (fmap (decimalDegrees . fromIntegral) navPointRwdirParser)
    <* char ','
    <*> optionMaybe (fmap metres navPointRwlenParser)
    <* char ','
    <*> optionMaybe navPointFreqParser
    <* char ','
    <*> fmap toText (inQuotations $ many1 $ noneOf ['"'])

navPointNameParser :: Parsec String () Text
navPointNameParser =
    toText <$> inQuotations (many1 $ noneOf ['"'])

navPointCodeParser :: Parsec String () Text
navPointCodeParser =
    toText 
    <$> choice 
            [ inQuotations (many1 $ noneOf ['"'])
            , many1 $ noneOf [',']
            ]

navPointCountryParser :: Parsec String () (Maybe Text)
navPointCountryParser =
    optionMaybe $ toText <$> many1 letter

navPointStyleParser :: Parsec String () WaypointStyle
navPointStyleParser =
    (<>) <$> digitAsString <*> option "" digitAsString >>= matchIdParser
    where
        digitAsString :: Parsec String () String
        digitAsString = one <$> digit

        matchIdParser :: String -> Parsec String () WaypointStyle
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
        matchIdParser id = fail $ "Failed to parse waypoint style. Unknown style: " <> id


navPointLatParser :: Parsec String () Double
navPointLatParser = do
    deg <- readEither <$> count 2 digit
    min <- readEither <$> count 2 digit
    char '.'
    decMin <- readEither <$> count 3 digit
    adjustForHemisphereFn <-
        choice
            [ id <$ char 'N'
            , negate <$ char 'S' -- need to negate the value for southern hemisphere
            ]

    case toAngle <$> deg <*> min <*> decMin of
        Right x -> pure $ adjustForHemisphereFn x
        Left e -> fail $ "Failed to parse latitude: " ++ toString e

    where
        toAngle deg min decMin =
            fromIntegral deg + fromIntegral min / 60 + fromIntegral decMin / 60000

navPointLonParser :: Parsec String () Double
navPointLonParser = do
    deg <- readEither <$> count 3 digit
    min <- readEither <$> count 2 digit
    char '.'
    decMin <- readEither <$> count 3 digit
    adjustForHemisphereFn <-
        choice
            [ id <$ char 'E'
            , negate <$ char 'W' -- need to negate the value for western hemisphere
            ]

    case toAngle <$> deg <*> min <*> decMin of
        Right x -> pure $ adjustForHemisphereFn x
        Left e -> fail $ "Failed to parse longitude: " ++ toString e
    where
        toAngle deg min decMin =
            fromIntegral deg + fromIntegral min / 60 + fromIntegral decMin / 60000

navPointElevationParser :: Parsec String () Double
navPointElevationParser = do
    elev <- doubleParser
    unitConversionFn <-
        choice
            [ id <$ char 'm' -- meters
            , (/3.2808) <$ string "ft" -- feet
            ]

    pure $ unitConversionFn elev


navPointRwdirParser :: Parsec String () Int
navPointRwdirParser = do
    dir <- readEither <$> count 1 (oneOf ['0', '1', '2', '3']) <> count 2 digit

    case dir of
        Right x -> pure x
        Left e -> fail $ "Failed to parse rwdir: " ++ toString e

navPointRwlenParser :: Parsec String () Double
navPointRwlenParser = do
    len <- doubleParser
    unit <-
        choice
            [ char 'm' >> option id ml  -- meters 'm' or statute miles 'ml'
            , (* 1852) <$ string "nm" -- nautical miles
            ]

    pure $ unit len

    where
        ml = (* 1609.344) <$ string "l" -- statute miles

navPointFreqParser :: Parsec String () Text
navPointFreqParser = 
    toText
    <$> choice [inQuotations freq, freq]
    where 
        freq :: Parsec String () String
        freq = 
            count 1 (char '1') 
            <> count 1 (oneOf ['1', '2', '3'])
            <> count 1 digit 
            <> count 1 (char '.')
            <> count 2 digit 
            <> count 1 (oneOf ['0', '5'])

intParser :: Parsec String () Int
intParser = do
    res <- readEither <$> many1 digit
    case res of
        Right i -> pure i
        Left e -> fail $ "Failed to parse int: " ++ toString e

fractionParser :: Parsec String u String
fractionParser = count 1 (char '.') <> many1 digit

doubleParser :: Parsec String () Double
doubleParser = do
    sign <- optionMaybe $ string "-"
    integral <- Just <$> many1 digit
    fractional <- optionMaybe fractionParser

    case readEither $ fold $ sign <> integral <> fractional of
        Right x -> pure x
        Left e -> fail $ "Failed to parse double: " ++ toString e


inQuotations :: Parsec String () String -> Parsec String () String
inQuotations =
    between (char '"') (char '"')

