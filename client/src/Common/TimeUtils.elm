module Common.TimeUtils exposing (addTime, formatTime, padInt)

import Time exposing (Posix)


addTime : Posix -> Posix -> Posix
addTime x y =
    Time.posixToMillis x + Time.posixToMillis y |> Time.millisToPosix


padInt : Int -> Int -> String
padInt len x =
    let
        unpadded : String
        unpadded =
            String.fromInt x
    in
    String.padLeft (len - String.length unpadded) '0' unpadded


formatTime : Posix -> String
formatTime x =
    (Time.toHour Time.utc >> String.fromInt >> String.padLeft 2 '0') x
        ++ ":"
        ++ (Time.toMinute Time.utc >> String.fromInt >> String.padLeft 2 '0') x
        ++ ":"
        ++ (Time.toSecond Time.utc >> String.fromInt >> String.padLeft 2 '0') x
