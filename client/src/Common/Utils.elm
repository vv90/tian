module Common.Utils exposing (..)


roundN : Int -> Float -> Float
roundN places n =
    let
        factor =
            toFloat (10 ^ places)
    in
    toFloat (round (n * factor)) / factor
