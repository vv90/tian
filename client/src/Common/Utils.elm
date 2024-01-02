module Common.Utils exposing (roundN, sinh)


roundN : Int -> Float -> Float
roundN places n =
    let
        factor =
            toFloat (10 ^ places)
    in
    toFloat (round (n * factor)) / factor


sinh : Float -> Float
sinh x =
    (e ^ x - e ^ -x) / 2
