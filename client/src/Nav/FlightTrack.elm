module Nav.FlightTrack exposing (..)

import Time exposing (..)
import Parser exposing (..)
import ParserUtils exposing (..)
import Date exposing (Date(..), toPosix)
import Result.Extra as ResultX
import Maybe.Extra as MaybeX
import Date exposing (formatDate)
import Geo.GeoUtils exposing (Latitude(..), Longitude(..), Altitude(..), getLat, getLon, getAltitude, toDecimalDegrees)
import Nav.Units exposing (Meters(..), Deg(..), getDeg, getMeters)
import Array exposing (Array)

type FixValidity = Gps3D | Baro2D
type alias TrackPoint = 
  { time: Posix
  , latitude: Latitude
  , longitude: Longitude
  , fixValidity: FixValidity
  , altitudeBaro: Altitude
  , altitudeGps: Altitude
  }

showTrackPoint : TrackPoint -> String
showTrackPoint tp =
  (toHour Time.utc >> String.fromInt) tp.time
  ++ ":"
  ++ (toMinute Time.utc >> String.fromInt) tp.time
  ++ ":"
  ++ (toSecond Time.utc >> String.fromInt) tp.time
  ++ " | " 
  ++ (getLat >> getDeg >> String.fromFloat) tp.latitude 
  ++ ", "
  ++ (getLon >> getDeg >> String.fromFloat) tp.longitude
  ++ " | "
  ++ (getAltitude >> getMeters >> String.fromFloat) tp.altitudeBaro
  ++ "m"

type FlightInfo 
  = FlightDate Posix
  | CompId String
  | Fix TrackPoint
  | UnknownRecord

showFlightInfo : FlightInfo -> String
showFlightInfo fi =
  case fi of
    FlightDate d -> formatDate d
    CompId id -> id
    Fix tp -> showTrackPoint tp
    UnknownRecord -> "???"

type alias FlightTrack = 
  { date: Maybe Posix
  , compId: Maybe String
  , points: Array TrackPoint
  }



showFlightTrack : FlightTrack -> String
showFlightTrack ft =
  MaybeX.unwrap "---" formatDate ft.date
  ++ " | " ++ MaybeX.unwrap "--" identity ft.compId
  ++ " | " ++ (Array.length >> String.fromInt) ft.points
  ++ " points \n"
  -- ++ (Array.map showTrackPoint >> Array.toList >> String.join "\n") ft.points

type TrackLoadError 
  = NoFile
  | TrackParsingError (List DeadEnd)


intToMonth : Int -> Parser Month
intToMonth m =
  case m of
    1 -> succeed Jan
    2 -> succeed Feb
    3 -> succeed Mar
    4 -> succeed Apr
    5 -> succeed May
    6 -> succeed Jun
    7 -> succeed Jul
    8 -> succeed Aug
    9 -> succeed Sep
    10 -> succeed Oct
    11 -> succeed Nov
    12 -> succeed Dec
    _ -> problem ("Failed to convert Int to Month. Invalid value: " ++ String.fromInt m)

trackDateParser : Parser FlightInfo
trackDateParser = 
  succeed 
    (\d m y -> Date (y + 2000) m d |> (toPosix >> FlightDate) )
    |. symbol "HFDTE"
    |= (digits 2 |> andThen digitsToInt)
    |= (digits 2 |> andThen digitsToInt |> andThen intToMonth)
    |= (digits 2 |> andThen digitsToInt)
    |. chompUntil "\n"

compIdParser : Parser FlightInfo
compIdParser =
  succeed
    CompId
    |. symbol "HFCIDCOMPETITIONID:"
    |= (chompUntil "\n" |> getChompedString)

fixValidityParser : Parser FixValidity
fixValidityParser =
  oneOf 
  [ succeed Gps3D |. symbol "A"
  , succeed Baro2D |. symbol "V"
  ]

altitudeParser : Parser Altitude
altitudeParser =
  oneOf 
  [ succeed (negate >> toFloat >> Meters >> Altitude) |. symbol "-" |= (digits 4 |> andThen digitsToInt)
  , succeed (toFloat >> Meters >> Altitude) |= (digits 5 |> andThen digitsToInt)
  ]

trackFixLatParser : Parser Latitude
trackFixLatParser = 
  succeed (\d m mf f -> toDecimalDegrees d (toFloat m + toFloat mf / 1000 ) 0 |> f |> Deg |> LatDeg)
    |= (digits 2 |> andThen digitsToInt)
    |= (digits 2 |> andThen digitsToInt)
    |= (digits 3 |> andThen digitsToInt)
    |= oneOf 
        [ succeed identity |. symbol "N" 
        , succeed negate |. symbol "S"
        ]

trackFixLonParser : Parser Longitude
trackFixLonParser =
  succeed 
    (\d m mf f -> toDecimalDegrees d (toFloat m + toFloat mf / 1000 ) 0 |> f |> Deg |> LonDeg)
    |= (digits 3 |> andThen digitsToInt)
    |= (digits 2 |> andThen digitsToInt)
    |= (digits 3 |> andThen digitsToInt)
    |= oneOf 
        [ succeed identity |. symbol "E"
        , succeed negate |. symbol "W"
        ]

trackPointParser : Parser TrackPoint
trackPointParser =
  succeed (TrackPoint)
  |. symbol "B"
  |= timeOfDayParser
  |= trackFixLatParser
  |= trackFixLonParser
  |= fixValidityParser
  |= altitudeParser
  |= altitudeParser
  |. chompUntil "\n"



unknownRecordParser : Parser FlightInfo
unknownRecordParser = 
  oneOf
    [ succeed UnknownRecord |. symbol "H" |. chompUntil "\n"
    , succeed UnknownRecord |. symbol "A" |. chompUntil "\n"
    , succeed UnknownRecord |. symbol "I" |. chompUntil "\n"
    , succeed UnknownRecord |. symbol "J" |. chompUntil "\n"
    , succeed UnknownRecord |. symbol "C" |. chompUntil "\n"
    , succeed UnknownRecord |. symbol "D" |. chompUntil "\n"
    , succeed UnknownRecord |. symbol "E" |. chompUntil "\n"
    , succeed UnknownRecord |. symbol "F" |. chompUntil "\n"
    , succeed UnknownRecord |. symbol "B" |. chompUntil "\n"
    , succeed UnknownRecord |. symbol "K" |. chompUntil "\n"
    , succeed UnknownRecord |. symbol "L" |. chompUntil "\n"
    , succeed UnknownRecord |. symbol "G" |. chompUntil "\n"
    , succeed UnknownRecord |. symbol "O" |. chompUntil "\n"
    , succeed UnknownRecord |. int
    ]

flightInfoParser : Parser FlightInfo
flightInfoParser =
  oneOf 
    [ trackDateParser
    , compIdParser
    , map Fix trackPointParser
    , unknownRecordParser
    ]

emptyFlightTrack : FlightTrack
emptyFlightTrack = 
  { date = Nothing
  , compId = Nothing
  , points = Array.empty
  }

withFlightInfo : FlightInfo -> FlightTrack -> FlightTrack
withFlightInfo info ft = 
  case info of
    FlightDate d -> { ft | date = Just d }
    CompId id -> { ft | compId = Just id }
    Fix tp -> { ft | points = Array.push tp ft.points }
    UnknownRecord -> ft

aggregateFlightInfo : List FlightInfo -> FlightTrack
aggregateFlightInfo = 
  List.foldr withFlightInfo emptyFlightTrack

parseTrackFile : List String -> Result (List DeadEnd) FlightTrack
parseTrackFile = 
  List.map (Parser.run flightInfoParser) >> ResultX.combine >> Result.map aggregateFlightInfo

flightTrackParser : Parser FlightTrack
flightTrackParser =
  loop emptyFlightTrack flightTrackParserStep

flightTrackParserStep : FlightTrack -> Parser (Step FlightTrack FlightTrack)
flightTrackParserStep track =
  oneOf 
    [ succeed (\x -> Loop (withFlightInfo x track))
      |= flightInfoParser |. symbol "\n" 
    , succeed (Done track)
      |. symbol "\n"
    , succeed (Done track)
      |. symbol ""
    ]