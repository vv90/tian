module Utils exposing (..)

import Parser exposing (DeadEnd)


showParseResult : Result (List DeadEnd) a -> (a -> String) -> String
showParseResult res f =
    case res of
        Err [] ->
            "No input parsed"

        Err deadEnds ->
            "Errors: " ++ Debug.toString deadEnds

        Ok r ->
            f r


showParseErrors : List DeadEnd -> String
showParseErrors ers =
    Debug.toString ers
