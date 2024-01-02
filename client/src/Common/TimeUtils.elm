module Common.TimeUtils exposing (addTime, formatTime, padInt)

import Time exposing (..)


addTime : Posix -> Posix -> Posix
addTime x y =
    posixToMillis x + posixToMillis y |> millisToPosix


padInt : Int -> Int -> String
padInt len x =
    let
        unpadded =
            String.fromInt x
    in
    String.padLeft (len - String.length unpadded) '0' unpadded


formatTime : Posix -> String
formatTime x =
    (toHour utc >> String.fromInt >> String.padLeft 2 '0') x
        ++ ":"
        ++ (toMinute utc >> String.fromInt >> String.padLeft 2 '0') x
        ++ ":"
        ++ (toSecond utc >> String.fromInt >> String.padLeft 2 '0') x
