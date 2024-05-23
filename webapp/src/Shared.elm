module Shared exposing
    ( defaultPage
    , Flags, decoder
    , Model, Msg
    , init, update, subscriptions
    )

{-|

@docs defaultPage
@docs Flags, decoder
@docs Model, Msg
@docs init, update, subscriptions

-}

import Browser.Events
import Dict
import Effect exposing (Effect)
import FreeLayout2
import Json.Decode
import Route exposing (Route)
import Route.Path
import Shared.Model
import Shared.Msg


defaultPage : { path : Route.Path.Path, query : Dict.Dict String String, hash : Maybe String }
defaultPage =
    { path = Route.Path.Radar, query = Dict.empty, hash = Nothing }



-- FLAGS


type alias Flags =
    { windowSize : FreeLayout2.WindowSize }


decoder : Json.Decode.Decoder Flags
decoder =
    Json.Decode.map Flags
        (Json.Decode.field "windowSize" FreeLayout2.windowSizeDecoder)



-- INIT


type alias Model =
    Shared.Model.Model


layoutConfig : FreeLayout2.LayoutConfig
layoutConfig =
    { mobileScreen =
        { minGridWidth = 360
        }
    , desktopScreen =
        { minGridWidth = 1024
        }
    }


init : Result Json.Decode.Error Flags -> Route () -> ( Model, Effect Msg )
init flagsResult _ =
    case flagsResult of
        Ok flags ->
            ( { layout = FreeLayout2.init layoutConfig flags.windowSize }, Effect.none )

        Err _ ->
            -- TODO: handle error
            ( { layout = FreeLayout2.init layoutConfig { width = 1024, height = 768 } }, Effect.none )



-- UPDATE


type alias Msg =
    Shared.Msg.Msg


update : Route () -> Msg -> Model -> ( Model, Effect Msg )
update _ msg model =
    case msg of
        Shared.Msg.GotNewWindowSize newWindowSize ->
            ( { model | layout = FreeLayout2.update model.layout newWindowSize }, Effect.none )



-- SUBSCRIPTIONS


subscriptions : Route () -> Model -> Sub Msg
subscriptions route model =
    Browser.Events.onResize (\width height -> Shared.Msg.GotNewWindowSize { width = width, height = height })
