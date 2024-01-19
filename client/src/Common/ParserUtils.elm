module Common.ParserUtils exposing (digits, digitsHelp, digitsToInt, inQuotations, maybeParser, plainFloatParser, plainIntParser, timeOfDayParser, wordVal)

import Parser exposing ((|.), (|=), Parser, Step(..))
import Set
import Time exposing (Posix, millisToPosix)


wordVal : Parser String
wordVal =
    Parser.variable
        { start = Char.isAlpha
        , inner = Char.isAlpha
        , reserved = Set.empty
        }


inQuotations : Parser a -> Parser a
inQuotations internal =
    Parser.succeed identity
        |. Parser.symbol "\""
        |= internal
        |. Parser.symbol "\""


digits : Int -> Parser String
digits n =
    Parser.loop ( n, "" ) digitsHelp


digitsHelp : ( Int, String ) -> Parser (Step ( Int, String ) String)
digitsHelp ( n, s ) =
    if n > 0 then
        Parser.getChompedString
            (Parser.succeed () |. Parser.chompIf Char.isDigit)
            |> Parser.map
                (String.append s >> Tuple.pair (n - 1) >> Loop)

    else
        Parser.succeed ()
            |> Parser.map (\_ -> Done s)


digitsToInt : String -> Parser Int
digitsToInt ds =
    case String.toInt ds of
        Just x ->
            Parser.succeed x

        Nothing ->
            Parser.problem "Failed to convert digits to int"


plainFloatParser : Parser Float
plainFloatParser =
    Parser.variable
        { start = \x -> Char.isDigit x || x == '.'
        , inner = \x -> Char.isDigit x || x == '.'
        , reserved = Set.empty
        }
        |> Parser.andThen
            (\s ->
                case String.toFloat s of
                    Just x ->
                        Parser.succeed x

                    Nothing ->
                        Parser.problem "Failed to parse float"
            )


plainIntParser : Parser Int
plainIntParser =
    Parser.variable
        { start = Char.isDigit
        , inner = Char.isDigit
        , reserved = Set.empty
        }
        |> Parser.andThen digitsToInt


maybeParser : Parser a -> Parser (Maybe a)
maybeParser p =
    Parser.oneOf
        [ Parser.succeed Just |= p
        , Parser.succeed Nothing
        ]


timeOfDayParser : Parser Posix
timeOfDayParser =
    Parser.succeed (\hr min sec -> millisToPosix (sec * 1000 + min * 60000 + hr * 3600000))
        |= (digits 2 |> Parser.andThen digitsToInt)
        |= (digits 2 |> Parser.andThen digitsToInt)
        |= (digits 2 |> Parser.andThen digitsToInt)
