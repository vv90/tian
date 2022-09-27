module Nav.FlightTrack exposing (..)

import Array exposing (Array)
import Geo.GeoUtils exposing (Altitude(..), GeoPoint, Latitude(..), Longitude(..), getAltitude, getLat, getLon, toDecimalDegrees)
import List.Nonempty as Nonempty exposing (Nonempty(..))
import Maybe.Extra as MaybeX
import Nav.Units exposing (Deg(..), Meters(..), getDeg, getMeters)
import Parser exposing (..)
import Result.Extra as ResultX
import Time exposing (..)
import Utils.Date exposing (Date(..), formatDate, toPosix)
import Utils.ParserUtils exposing (..)


type FixValidity
    = Gps3D
    | Baro2D


type alias TrackPoint =
    { time : Posix
    , latitude : Latitude
    , longitude : Longitude
    , fixValidity : FixValidity
    , altitudeBaro : Altitude
    , altitudeGps : Altitude
    }


trackPointToGeoPoint : TrackPoint -> GeoPoint
trackPointToGeoPoint tp =
    GeoPoint tp.longitude tp.latitude


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
        FlightDate d ->
            formatDate d

        CompId id ->
            id

        Fix tp ->
            showTrackPoint tp

        UnknownRecord ->
            "???"


type alias FlightTrack =
    { date : Posix
    , compId : String
    , points : Nonempty TrackPoint
    }


type alias FlightTrackAggregateValue =
    ( Maybe Posix, Maybe String, Array TrackPoint )


type FlightTrackReadError
    = ParseError (List DeadEnd)
    | MissingData String


showFlightTrackReadError : FlightTrackReadError -> String
showFlightTrackReadError error =
    case error of
        ParseError e ->
            Debug.toString e

        MissingData msg ->
            msg


showFlightTrack : FlightTrack -> String
showFlightTrack ft =
    formatDate ft.date
        ++ " | "
        ++ ft.compId
        ++ " | "
        ++ (Nonempty.length >> String.fromInt) ft.points
        ++ " points \n"



-- ++ (Array.map showTrackPoint >> Array.toList >> String.join "\n") ft.points


intToMonth : Int -> Parser Month
intToMonth m =
    case m of
        1 ->
            succeed Jan

        2 ->
            succeed Feb

        3 ->
            succeed Mar

        4 ->
            succeed Apr

        5 ->
            succeed May

        6 ->
            succeed Jun

        7 ->
            succeed Jul

        8 ->
            succeed Aug

        9 ->
            succeed Sep

        10 ->
            succeed Oct

        11 ->
            succeed Nov

        12 ->
            succeed Dec

        _ ->
            problem ("Failed to convert Int to Month. Invalid value: " ++ String.fromInt m)


trackDateParser : Parser FlightInfo
trackDateParser =
    succeed
        (\d m y -> Date (y + 2000) m d |> (toPosix >> FlightDate))
        |. oneOf
            [ succeed () |. symbol "HFDTEDATE:"
            , succeed () |. symbol "HFDTE"
            ]
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
    succeed (\d m mf f -> toDecimalDegrees d (toFloat m + toFloat mf / 1000) 0 |> f |> Deg |> LatDeg)
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
        (\d m mf f -> toDecimalDegrees d (toFloat m + toFloat mf / 1000) 0 |> f |> Deg |> LonDeg)
        |= (digits 3 |> andThen digitsToInt)
        |= (digits 2 |> andThen digitsToInt)
        |= (digits 3 |> andThen digitsToInt)
        |= oneOf
            [ succeed identity |. symbol "E"
            , succeed negate |. symbol "W"
            ]


trackPointParser : Parser TrackPoint
trackPointParser =
    succeed TrackPoint
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


withFlightInfo : FlightInfo -> ( Maybe Posix, Maybe String, Array TrackPoint ) -> ( Maybe Posix, Maybe String, Array TrackPoint )
withFlightInfo info ( date, compId, points ) =
    case info of
        FlightDate d ->
            ( Just d, compId, points )

        CompId id ->
            ( date, Just id, points )

        Fix tp ->
            ( date, compId, Array.push tp points )

        UnknownRecord ->
            ( date, compId, points )



-- aggregateFlightInfo : List FlightInfo -> Result FlightTrackReadError FlightTrack
-- aggregateFlightInfo fiItems =
--   let
--     aggregate =
--       List.foldr withFlightInfo (Nothing, Nothing, Array.empty) fiItems
--       |> (\(date, compId, points) -> (date, compId, Array.toList points))
--   in
--     case aggregate of
--       (Just date, Just compId, p::ps) ->
--         Ok
--           { date = date
--           , compId = compId
--           , points = p::ps
--           }
--       (Nothing, _, _) ->
--         (MissingData >> Err) "Missing date"
--       (_, Nothing, _) ->
--         (MissingData >> Err) "Missing CompId"
--       (_, _, []) ->
--         (MissingData >> Err) "No track points"
-- parseTrackFile : List String -> Result FlightTrackReadError FlightTrack
-- parseTrackFile =
--   List.map (Parser.run flightInfoParser)
--   >> ResultX.combine
--   >> Result.mapError ParseError
--   >> Result.andThen aggregateFlightInfo


flightTrackParser : Parser FlightTrackAggregateValue
flightTrackParser =
    loop ( Nothing, Nothing, Array.empty ) flightTrackParserStep


flightTrackParserStep : FlightTrackAggregateValue -> Parser (Step FlightTrackAggregateValue FlightTrackAggregateValue)
flightTrackParserStep track =
    oneOf
        [ succeed (\x -> Loop (withFlightInfo x track))
            |= flightInfoParser
            |. symbol "\n"
        , succeed (Done track)
            |. symbol "\n"
        , succeed (Done track)
            |. symbol ""
        ]


validateFlightTrack : FlightTrackAggregateValue -> Result FlightTrackReadError FlightTrack
validateFlightTrack ( date, compId, points ) =
    case ( date, compId, Array.toList points ) of
        ( Just d, Just cid, p :: ps ) ->
            Ok
                { date = d
                , compId = cid
                , points = Nonempty p ps
                }

        ( Nothing, _, _ ) ->
            (MissingData >> Err) "Missing date"

        ( _, Nothing, _ ) ->
            (MissingData >> Err) "Missing CompId"

        ( _, _, [] ) ->
            (MissingData >> Err) "No track points"


parseFlightTrack : String -> Result FlightTrackReadError FlightTrack
parseFlightTrack =
    Parser.run flightTrackParser
        >> Result.mapError ParseError
        >> Result.andThen validateFlightTrack
