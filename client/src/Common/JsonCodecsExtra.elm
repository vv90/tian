module Common.JsonCodecsExtra exposing (..)

import File exposing (File)
import Json.Decode as D
import Json.Encode as E


filesDecoder : D.Decoder (List File)
filesDecoder =
    D.at [ "target", "files" ] (D.list File.decoder)


tupleDecoder : ( D.Decoder a, D.Decoder b ) -> D.Decoder ( a, b )
tupleDecoder ( decodeA, decodeB ) =
    D.map2 Tuple.pair
        (D.index 0 decodeA)
        (D.index 1 decodeB)


tupleEncoder : ( a -> E.Value, b -> E.Value ) -> ( a, b ) -> E.Value
tupleEncoder ( encodeA, encodeB ) ( a, b ) =
    E.list
        identity
        [ encodeA a
        , encodeB b
        ]


tripleDecoder : ( D.Decoder a, D.Decoder b, D.Decoder c ) -> D.Decoder ( a, b, c )
tripleDecoder ( decodeA, decodeB, decodeC ) =
    D.map3
        (\a b c -> ( a, b, c ))
        (D.index 0 decodeA)
        (D.index 1 decodeB)
        (D.index 2 decodeC)
