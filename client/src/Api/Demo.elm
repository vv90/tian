module Api.Demo exposing
    ( NameMatch
    , nameMatchDecoder
    , nameMatchEncoder
    )

import Json.Decode
import Json.Decode.Pipeline
import Json.Encode


type alias NameMatch =
    { compId : String, name : String }


nameMatchEncoder : NameMatch -> Json.Encode.Value
nameMatchEncoder a =
    Json.Encode.object
        [ ( "compId", Json.Encode.string a.compId )
        , ( "name", Json.Encode.string a.name )
        ]


nameMatchDecoder : Json.Decode.Decoder NameMatch
nameMatchDecoder =
    Json.Decode.succeed NameMatch
        |> Json.Decode.Pipeline.required "compId" Json.Decode.string
        |> Json.Decode.Pipeline.required "name" Json.Decode.string
