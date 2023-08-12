module Api.Entity exposing
    ( Entity
    , entityEncoder
    , entityDecoder
    )

import Json.Decode
import Json.Decode.Pipeline
import Json.Encode


type alias Entity a b =
    { key : a, entity : b }


entityEncoder : (a -> Json.Encode.Value) -> (b -> Json.Encode.Value) -> Entity a b -> Json.Encode.Value
entityEncoder a b c =
    Json.Encode.object [("key" , a c.key), ("entity" , b c.entity)]


entityDecoder : Json.Decode.Decoder a -> Json.Decode.Decoder b -> Json.Decode.Decoder (Entity a b)
entityDecoder a b =
    Json.Decode.succeed Entity |>
    Json.Decode.Pipeline.required "key" a |>
    Json.Decode.Pipeline.required "entity" b