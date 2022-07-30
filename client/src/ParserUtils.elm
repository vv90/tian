module ParserUtils exposing (..)

import Geo.GeoUtils exposing (Latitude(..), Longitude(..), toDecimalDegrees)
import Nav.Units exposing (Deg(..))
import Parser exposing (..)
import Set
import Time exposing (Posix, millisToPosix)


wordVal : Parser String
wordVal =
    variable
        { start = Char.isAlpha
        , inner = Char.isAlpha
        , reserved = Set.empty
        }


inQuotations : Parser a -> Parser a
inQuotations internal =
    succeed identity
        |. symbol "\""
        |= internal
        |. symbol "\""


digits : Int -> Parser String
digits n =
    loop ( n, "" ) digitsHelp


digitsHelp : ( Int, String ) -> Parser (Step ( Int, String ) String)
digitsHelp ( n, s ) =
    if n > 0 then
        getChompedString
            (succeed () |. chompIf Char.isDigit)
            |> Parser.map
                (String.append s >> Tuple.pair (n - 1) >> Loop)

    else
        succeed ()
            |> Parser.map (\_ -> Done s)


digitsToInt : String -> Parser Int
digitsToInt ds =
    case String.toInt ds of
        Just x ->
            succeed x

        Nothing ->
            problem "Failed to convert digits to int"


plainFloatParser : Parser Float
plainFloatParser =
    variable
        { start = \x -> Char.isDigit x || x == '.'
        , inner = \x -> Char.isDigit x || x == '.'
        , reserved = Set.empty
        }
        |> andThen
            (\s ->
                case String.toFloat s of
                    Just x ->
                        succeed x

                    Nothing ->
                        problem "Failed to parse float"
            )


plainIntParser : Parser Int
plainIntParser =
    variable
        { start = Char.isDigit
        , inner = Char.isDigit
        , reserved = Set.empty
        }
        |> andThen digitsToInt


maybeParser : Parser a -> Parser (Maybe a)
maybeParser p =
    oneOf
        [ succeed Just |= p
        , succeed Nothing
        ]


latParser : Parser Latitude
latParser =
    succeed (\d m f -> toDecimalDegrees d m 0 |> f |> Deg |> LatDeg)
        |= (digits 2 |> andThen digitsToInt)
        |= plainFloatParser
        |= oneOf
            [ succeed identity |. symbol "N"
            , succeed negate |. symbol "S"
            ]


lonParser : Parser Longitude
lonParser =
    succeed
        (\d m f -> toDecimalDegrees d m 0 |> f |> Deg |> LonDeg)
        |= (digits 3 |> andThen digitsToInt)
        |= plainFloatParser
        |= oneOf
            [ succeed identity |. symbol "E"
            , succeed negate |. symbol "W"
            ]


timeOfDayParser : Parser Posix
timeOfDayParser =
    succeed (\hr min sec -> millisToPosix (sec * 1000 + min * 60000 + hr * 3600000))
        |= (digits 2 |> andThen digitsToInt)
        |= (digits 2 |> andThen digitsToInt)
        |= (digits 2 |> andThen digitsToInt)
