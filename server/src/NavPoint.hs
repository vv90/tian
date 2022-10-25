

module NavPoint
    where

import Relude
import qualified Data.Aeson as Aeson
import GHC.Generics ( Generic )
import Data.Int (Int32)
import Data.Text (Text, pack, unpack)
import Text.Parsec (Parsec, oneOf, digit, many1, count, between, alphaNum, choice, string, letter, optionMaybe, option, noneOf, sepEndBy1, parserTrace, parserTraced, eof)
import Text.Parsec.Char (char, digit)
-- import Data.Geo.Jord.Angle (Angle, decimalDegrees)
-- import Data.Geo.Jord.Length (Length, metres)
-- import Data.Geo.Jord.Geodetic (Position, latLongHeightPos)
-- import Data.Geo.Jord.Models (WGS84 (WGS84))
import Data.Char (digitToInt)
import Text.Parsec.Pos (updatePosString, updatePosChar)
import Text.Parsec.Prim (tokenPrim, token, tokens, (<?>))
import qualified Generics.SOP as SOP
import Language.Haskell.To.Elm (HasElmEncoder, HasElmDecoder, HasElmType)
import Magic.ElmDeriving

roundN :: (RealFrac a, Integral b) => b -> a -> a
roundN n x = (fromIntegral . round $ x * f) / f
    where f = 10^n

-- Degrees Decimal Minutes (DDM) to Decimal Degrees (DD)
ddmTodd :: (RealFrac b, Integral a) => a -> a -> a -> b
ddmTodd deg min decMin =
    -- since the maximum precision of the input is 0.001' ~ 0.000017° we round to 6 decimal places
    roundN 6 $ fromIntegral deg + fromIntegral min / 60 + fromIntegral decMin / 60000

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
    deriving (Show, Read, Eq, Generic, SOP.Generic, SOP.HasDatatypeInfo, Aeson.ToJSON, Aeson.FromJSON)
    deriving (HasElmType, HasElmEncoder Aeson.Value, HasElmDecoder Aeson.Value)
        via ElmType "Api.NavPoint.WaypointStyle" WaypointStyle

newtype Latitude = LatitudeDegrees Double
    deriving (Show, Read, Eq, Generic, SOP.Generic, SOP.HasDatatypeInfo, Aeson.ToJSON, Aeson.FromJSON)
    deriving (HasElmType, HasElmEncoder Aeson.Value, HasElmDecoder Aeson.Value)
        via ElmType "Api.NavPoint.Latitude" Latitude

degreesLatitude :: Latitude -> Double
degreesLatitude (LatitudeDegrees x) = x

newtype Longitude = LongitudeDegrees Double
    deriving (Show, Read, Eq, Generic, SOP.Generic, SOP.HasDatatypeInfo, Aeson.ToJSON, Aeson.FromJSON)
    deriving (HasElmType, HasElmEncoder Aeson.Value, HasElmDecoder Aeson.Value)
        via ElmType "Api.NavPoint.Longitude" Longitude

degreesLongitude :: Longitude -> Double
degreesLongitude (LongitudeDegrees x) = x
newtype Elevation = ElevationMeters Double
    deriving (Show, Read, Eq, Generic, SOP.Generic, SOP.HasDatatypeInfo, Aeson.ToJSON, Aeson.FromJSON)
    deriving (HasElmType, HasElmEncoder Aeson.Value, HasElmDecoder Aeson.Value)
        via ElmType "Api.NavPoint.Elevation" Elevation

metersElevation :: Elevation -> Double
metersElevation (ElevationMeters x) = x
newtype Direction = DirectionDegrees Int32
    deriving (Show, Read, Eq, Generic, SOP.Generic, SOP.HasDatatypeInfo, Aeson.ToJSON, Aeson.FromJSON)
    deriving (HasElmType, HasElmEncoder Aeson.Value, HasElmDecoder Aeson.Value)
        via ElmType "Api.NavPoint.Direction" Direction

degreesDirection :: Direction -> Int32
degreesDirection (DirectionDegrees x) = x
newtype Length = LengthMeters Double
    deriving (Show, Read, Eq, Generic, SOP.Generic, SOP.HasDatatypeInfo, Aeson.ToJSON, Aeson.FromJSON)
    deriving (HasElmType, HasElmEncoder Aeson.Value, HasElmDecoder Aeson.Value)
        via ElmType "Api.NavPoint.Length" Length

metersLength :: Length -> Double
metersLength (LengthMeters x) = x

data NavPoint = NavPoint
    { name :: Text
    , code :: Text
    , country :: Maybe Text
    -- , position :: Position WGS84
    , lat :: Latitude
    , lon :: Longitude
    , elev :: Elevation
    , style :: WaypointStyle
    , rwdir :: Maybe Direction
    , rwlen :: Maybe Length
    , freq :: Maybe Text
    , desc :: Text
    } deriving (Show, Read, Eq, Generic, SOP.Generic, SOP.HasDatatypeInfo, Aeson.ToJSON, Aeson.FromJSON)
    deriving (HasElmType, HasElmEncoder Aeson.Value, HasElmDecoder Aeson.Value)
        via ElmType "Api.NavPoint.NavPoint" NavPoint

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
    toText <$> choice
        [ inQuotations (many1 $ noneOf ['"'])
        , many1 $ noneOf [',']
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
        matchIdParser id = fail $ "Failed to parse waypoint style. Unknown style: " <> id


navPointLatParser :: Parsec Text () Latitude
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

    case ddmTodd <$> deg <*> min <*> decMin of
        Right x -> pure $ LatitudeDegrees $ adjustForHemisphereFn x
        Left e -> fail $ "Failed to parse latitude: " ++ toString e

    
navPointLonParser :: Parsec Text () Longitude
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

    case ddmTodd <$> deg <*> min <*> decMin of
        Right x -> pure $ LongitudeDegrees $ adjustForHemisphereFn x
        Left e -> fail $ "Failed to parse longitude: " ++ toString e
    
navPointElevationParser :: Parsec Text () Elevation
navPointElevationParser = do
    elev <- doubleParser
    unitConversionFn <-
        choice
            [ id <$ char 'm' -- meters
            , (/3.2808) <$ string "ft" -- feet
            ]

    pure $ ElevationMeters $ unitConversionFn elev


navPointRwdirParser :: Parsec Text () Direction
navPointRwdirParser = do
    dir <- readEither <$> count 1 (oneOf ['0', '1', '2', '3']) <> count 2 digit

    case dir of
        Right x -> pure $ DirectionDegrees x
        Left e -> fail $ "Failed to parse rwdir: " ++ toString e

navPointRwlenParser :: Parsec Text () Length
navPointRwlenParser = do
    len <- doubleParser
    unit <-
        choice
            [ char 'm' >> option id ml  -- meters 'm' or statute miles 'ml'
            , (* 1852) <$ string "nm" -- nautical miles
            ]

    pure $ LengthMeters $ unit len

    where
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

