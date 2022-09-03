module Api.FlightTask exposing
    ( FlightTask
    , TaskFinish(..)
    , TaskStart(..)
    , Turnpoint(..)
    , flightTaskDecoder
    , flightTaskEncoder
    , taskFinishDecoder
    , taskFinishEncoder
    , taskStartDecoder
    , taskStartEncoder
    , turnpointDecoder
    , turnpointEncoder
    )

import Api.NavPoint
import Json.Decode
import Json.Decode.Pipeline
import Json.Encode


type Turnpoint
    = Cylinder Float


turnpointEncoder : Turnpoint -> Json.Encode.Value
turnpointEncoder a =
    case a of
        Cylinder b ->
            Json.Encode.float b


turnpointDecoder : Json.Decode.Decoder Turnpoint
turnpointDecoder =
    Json.Decode.map Cylinder Json.Decode.float


type TaskStart
    = StartLine Float


taskStartEncoder : TaskStart -> Json.Encode.Value
taskStartEncoder a =
    case a of
        StartLine b ->
            Json.Encode.float b


taskStartDecoder : Json.Decode.Decoder TaskStart
taskStartDecoder =
    Json.Decode.map StartLine Json.Decode.float


type TaskFinish
    = FinishLine Float


taskFinishEncoder : TaskFinish -> Json.Encode.Value
taskFinishEncoder a =
    case a of
        FinishLine b ->
            Json.Encode.float b


taskFinishDecoder : Json.Decode.Decoder TaskFinish
taskFinishDecoder =
    Json.Decode.map FinishLine Json.Decode.float


type alias FlightTask =
    { start : ( Api.NavPoint.NavPoint, TaskStart )
    , turnpoints : List ( Api.NavPoint.NavPoint, Turnpoint )
    , finish : ( Api.NavPoint.NavPoint, TaskFinish )
    }


flightTaskEncoder : FlightTask -> Json.Encode.Value
flightTaskEncoder a =
    Json.Encode.object
        [ ( "start"
          , case a.start of
                ( b, c ) ->
                    Json.Encode.list identity
                        [ Api.NavPoint.navPointEncoder b
                        , taskStartEncoder c
                        ]
          )
        , ( "turnpoints"
          , Json.Encode.list
                (\b ->
                    case b of
                        ( c, d ) ->
                            Json.Encode.list identity
                                [ Api.NavPoint.navPointEncoder c
                                , turnpointEncoder d
                                ]
                )
                a.turnpoints
          )
        , ( "finish"
          , case a.finish of
                ( b, c ) ->
                    Json.Encode.list identity
                        [ Api.NavPoint.navPointEncoder b
                        , taskFinishEncoder c
                        ]
          )
        ]


flightTaskDecoder : Json.Decode.Decoder FlightTask
flightTaskDecoder =
    Json.Decode.succeed FlightTask
        |> Json.Decode.Pipeline.required "start" (Json.Decode.map2 Tuple.pair (Json.Decode.index 0 Api.NavPoint.navPointDecoder) (Json.Decode.index 1 taskStartDecoder))
        |> Json.Decode.Pipeline.required "turnpoints" (Json.Decode.list (Json.Decode.map2 Tuple.pair (Json.Decode.index 0 Api.NavPoint.navPointDecoder) (Json.Decode.index 1 turnpointDecoder)))
        |> Json.Decode.Pipeline.required "finish" (Json.Decode.map2 Tuple.pair (Json.Decode.index 0 Api.NavPoint.navPointDecoder) (Json.Decode.index 1 taskFinishDecoder))
