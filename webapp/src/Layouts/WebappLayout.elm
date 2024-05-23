module Layouts.WebappLayout exposing (Model, Msg, Props, layout)

import Effect exposing (Effect)
import Element exposing (..)
import FreeLayout2
import Layout exposing (Layout)
import Route exposing (Route)
import Shared
import View exposing (View)


type alias Props =
    {}


layout : Props -> Shared.Model -> Route () -> Layout () Model Msg contentMsg
layout _ shared _ =
    Layout.new
        { init = init
        , update = update
        , view = view shared
        , subscriptions = subscriptions
        }



-- MODEL


type alias Model =
    {}


init : () -> ( Model, Effect Msg )
init _ =
    ( {}
    , Effect.none
    )



-- UPDATE


type Msg
    = ReplaceMe


update : Msg -> Model -> ( Model, Effect Msg )
update msg model =
    case msg of
        ReplaceMe ->
            ( model
            , Effect.none
            )


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none



-- VIEW


view : Shared.Model -> { toContentMsg : Msg -> contentMsg, content : View contentMsg, model : Model } -> View contentMsg
view shared { content } =
    { title = content.title

    -- TODO: add TextStyle to bodyAttributes
    -- TODO: add Toasts to bodyAttributes
    , attributes = FreeLayout2.bodyAttributes shared.layout ++ content.attributes
    , element =
        let
            viewSidebar : Element contentMsg
            viewSidebar =
                column [ width (px 200), spacing 20, alignTop, padding 20 ]
                    [ text "sidebar content"
                    ]
        in
        row [ width fill, height fill ]
            [ viewSidebar
            , el [ alignTop, width fill ] <| content.element
            ]
    }
