module Api.TaskProgress exposing
    ( ProgressPoint
    , progressPointDecoder
    , progressPointEncoder
    )

import Api.Geo
import Api.NavPoint
import Json.Decode
import Json.Decode.Pipeline
import Json.Encode
import Maybe.Extra


type alias ProgressPoint =
    { time : Int
    , lat : Api.Geo.Latitude
    , lon : Api.Geo.Longitude
    , altitude : Api.Geo.Elevation
    , target : Maybe ( Api.NavPoint.NavPoint, Api.Geo.Distance )
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
                                [ Api.NavPoint.navPointEncoder c
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
        |> Json.Decode.Pipeline.required "target" (Json.Decode.nullable (Json.Decode.map2 Tuple.pair (Json.Decode.index 0 Api.NavPoint.navPointDecoder) (Json.Decode.index 1 Api.Geo.distanceDecoder)))
