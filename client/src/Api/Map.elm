module Api.Map exposing
    ( Tile
    , tileEncoder
    , tileDecoder
    )

import Json.Decode
import Json.Decode.Pipeline
import Json.Encode


type alias Tile  =
    { x : Int, y : Int, zoom : Int }


tileEncoder : Tile -> Json.Encode.Value
tileEncoder a =
    Json.Encode.object [ ("x" , Json.Encode.int a.x)
    , ("y" , Json.Encode.int a.y)
    , ("zoom" , Json.Encode.int a.zoom) ]


tileDecoder : Json.Decode.Decoder Tile
tileDecoder =
    Json.Decode.succeed Tile |>
    Json.Decode.Pipeline.required "x" Json.Decode.int |>
    Json.Decode.Pipeline.required "y" Json.Decode.int |>
    Json.Decode.Pipeline.required "zoom" Json.Decode.int