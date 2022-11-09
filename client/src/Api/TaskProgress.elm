module Api.TaskProgress exposing
    ( ProgressPoint
    , TaskProgress
    , progressPointDecoder
    , progressPointEncoder
    , taskProgressDecoder
    , taskProgressEncoder
    )

import Api.Geo
import Iso8601
import Json.Decode
import Json.Decode.Pipeline
import Json.Encode
import Maybe.Extra
import Time


type alias ProgressPoint =
    { time : Int
    , lat : Api.Geo.Latitude
    , lon : Api.Geo.Longitude
    , altitude : Api.Geo.Elevation
    , target : Maybe ( String, Api.Geo.Distance )
    }


progressPointEncoder : ProgressPoint -> Json.Encode.Value
progressPointEncoder a =
    Json.Encode.object
        [ ( "time", Json.Encode.int a.time )
        , ( "lat", Api.Geo.latitudeEncoder a.lat )
        , ( "lon", Api.Geo.longitudeEncoder a.lon )
        , ( "altitude", Api.Geo.elevationEncoder a.altitude )
        , ( "target"
          , Maybe.Extra.unwrap Json.Encode.null
                (\b ->
                    case b of
                        ( c, d ) ->
                            Json.Encode.list identity
                                [ Json.Encode.string c
                                , Api.Geo.distanceEncoder d
                                ]
                )
                a.target
          )
        ]


progressPointDecoder : Json.Decode.Decoder ProgressPoint
progressPointDecoder =
    Json.Decode.succeed ProgressPoint
        |> Json.Decode.Pipeline.required "time" Json.Decode.int
        |> Json.Decode.Pipeline.required "lat" Api.Geo.latitudeDecoder
        |> Json.Decode.Pipeline.required "lon" Api.Geo.longitudeDecoder
        |> Json.Decode.Pipeline.required "altitude" Api.Geo.elevationDecoder
        |> Json.Decode.Pipeline.required "target" (Json.Decode.nullable (Json.Decode.map2 Tuple.pair (Json.Decode.index 0 Json.Decode.string) (Json.Decode.index 1 Api.Geo.distanceDecoder)))


type alias TaskProgress =
    { taskId : Int
    , date : Time.Posix
    , compId : String
    , points : List ProgressPoint
    }


taskProgressEncoder : TaskProgress -> Json.Encode.Value
taskProgressEncoder a =
    Json.Encode.object
        [ ( "taskId", Json.Encode.int a.taskId )
        , ( "date", Iso8601.encode a.date )
        , ( "compId", Json.Encode.string a.compId )
        , ( "points", Json.Encode.list progressPointEncoder a.points )
        ]


taskProgressDecoder : Json.Decode.Decoder TaskProgress
taskProgressDecoder =
    Json.Decode.succeed TaskProgress
        |> Json.Decode.Pipeline.required "taskId" Json.Decode.int
        |> Json.Decode.Pipeline.required "date" Iso8601.decoder
        |> Json.Decode.Pipeline.required "compId" Json.Decode.string
        |> Json.Decode.Pipeline.required "points" (Json.Decode.list progressPointDecoder)
