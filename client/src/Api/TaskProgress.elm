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
    , target : Maybe String
    , distance : Float
    , speed : Maybe Float
    }


progressPointEncoder : ProgressPoint -> Json.Encode.Value
progressPointEncoder a =
    Json.Encode.object
        [ ( "time", Json.Encode.int a.time )
        , ( "lat", Api.Geo.latitudeEncoder a.lat )
        , ( "lon", Api.Geo.longitudeEncoder a.lon )
        , ( "altitude", Api.Geo.elevationEncoder a.altitude )
        , ( "target", Maybe.Extra.unwrap Json.Encode.null Json.Encode.string a.target )
        , ( "distance", Json.Encode.float a.distance )
        , ( "speed", Maybe.Extra.unwrap Json.Encode.null Json.Encode.float a.speed )
        ]


progressPointDecoder : Json.Decode.Decoder ProgressPoint
progressPointDecoder =
    Json.Decode.succeed ProgressPoint
        |> Json.Decode.Pipeline.required "time" Json.Decode.int
        |> Json.Decode.Pipeline.required "lat" Api.Geo.latitudeDecoder
        |> Json.Decode.Pipeline.required "lon" Api.Geo.longitudeDecoder
        |> Json.Decode.Pipeline.required "altitude" Api.Geo.elevationDecoder
        |> Json.Decode.Pipeline.required "target" (Json.Decode.nullable Json.Decode.string)
        |> Json.Decode.Pipeline.required "distance" Json.Decode.float
        |> Json.Decode.Pipeline.required "speed" (Json.Decode.nullable Json.Decode.float)


type alias TaskProgress =
    { taskId : Int
    , date : Time.Posix
    , compId : String
    , points : List ProgressPoint
    , legs : List ( Api.Geo.Latitude, Api.Geo.Longitude )
    }


taskProgressEncoder : TaskProgress -> Json.Encode.Value
taskProgressEncoder a =
    Json.Encode.object
        [ ( "taskId", Json.Encode.int a.taskId )
        , ( "date", Iso8601.encode a.date )
        , ( "compId", Json.Encode.string a.compId )
        , ( "points", Json.Encode.list progressPointEncoder a.points )
        , ( "legs"
          , Json.Encode.list
                (\b ->
                    case b of
                        ( c, d ) ->
                            Json.Encode.list identity
                                [ Api.Geo.latitudeEncoder c
                                , Api.Geo.longitudeEncoder d
                                ]
                )
                a.legs
          )
        ]


taskProgressDecoder : Json.Decode.Decoder TaskProgress
taskProgressDecoder =
    Json.Decode.succeed TaskProgress
        |> Json.Decode.Pipeline.required "taskId" Json.Decode.int
        |> Json.Decode.Pipeline.required "date" Iso8601.decoder
        |> Json.Decode.Pipeline.required "compId" Json.Decode.string
        |> Json.Decode.Pipeline.required "points" (Json.Decode.list progressPointDecoder)
        |> Json.Decode.Pipeline.required "legs" (Json.Decode.list (Json.Decode.map2 Tuple.pair (Json.Decode.index 0 Api.Geo.latitudeDecoder) (Json.Decode.index 1 Api.Geo.longitudeDecoder)))
