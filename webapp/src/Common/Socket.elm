module Common.Socket exposing
    ( CloseEvent
    , MessageEvent
    , closeEventDecoder
    , closeEventEncoder
    , messageEventDecoder
    , messageEventEncoder
    )

import Json.Decode as D
import Json.Encode as E


type alias CloseEvent =
    { wasClean : Bool
    , code : Int
    , reason : String
    }


type alias MessageEvent a =
    { data : a
    , origin : String
    , lastEventId : String
    }


closeEventEncoder : CloseEvent -> E.Value
closeEventEncoder evt =
    E.object
        [ ( "wasClean", E.bool evt.wasClean )
        , ( "code", E.int evt.code )
        , ( "reason", E.string evt.reason )
        ]


closeEventDecoder : D.Decoder CloseEvent
closeEventDecoder =
    D.map3 CloseEvent
        (D.field "wasClean" D.bool)
        (D.field "code" D.int)
        (D.field "reason" D.string)


messageEventEncoder : (a -> E.Value) -> MessageEvent a -> E.Value
messageEventEncoder encodeData evt =
    E.object
        [ ( "data", encodeData evt.data )
        , ( "origin", E.string evt.origin )
        , ( "lastEventId", E.string evt.lastEventId )
        ]


messageEventDecoder : D.Decoder a -> D.Decoder (MessageEvent a)
messageEventDecoder decodeData =
    D.map3 MessageEvent
        (D.field "data" decodeData)
        (D.field "origin" D.string)
        (D.field "lastEventId" D.string)
