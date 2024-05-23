module Pages.Radar exposing (Model, Msg, page)

import Effect exposing (Effect)
import Element exposing (..)
import Element.Background as Background
import Layouts
import Page exposing (Page)
import Palette
import Ports
import Route exposing (Route)
import Shared
import View exposing (View)


page : Shared.Model -> Route () -> Page Model Msg
page shared _ =
    Page.new
        { init = init
        , update = update
        , subscriptions = always Sub.none
        , view = view shared
        }
        |> Page.withLayout (always <| Layouts.WebappLayout {})


type alias Model =
    {}


init : () -> ( Model, Effect Msg )
init _ =
    ( {}
    , Effect.none
    )


type alias Msg =
    ()


update : Msg -> Model -> ( Model, Effect Msg )
update msg model =
    ( model, Effect.none )


view : Shared.Model -> Model -> View Msg
view { layout } model =
    { title = "Homepage"
    , attributes = []
    , element =
        column
            [ width <| px layout.window.width
            , height <| px layout.window.height
            , Background.color <| Palette.primary
            ]
            [ text <| String.fromInt layout.window.width
            , text <| String.fromInt layout.window.height
            ]
    }
