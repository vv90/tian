module Pages.Radar exposing (Model, Msg, page)

import Api.Types exposing (Latitude(..), Longitude(..))
import Components.Map3d as Map3d exposing (view)
import Effect exposing (Effect)
import Element exposing (..)
import Element.Background as Background
import Layouts
import Page exposing (Page)
import Palette
import Route exposing (Route)
import Shared
import View exposing (View)


page : Shared.Model -> Route () -> Page Model Msg
page shared _ =
    Page.new
        { init = init shared
        , update = update
        , subscriptions = subscriptions
        , view = view shared
        }
        |> Page.withLayout (always <| Layouts.WebappLayout {})


type alias Model =
    { map3d : Map3d.Model }


init : Shared.Model -> () -> ( Model, Effect Msg )
init shared _ =
    let
        origin =
            { lat = LatitudeDegrees 45.208451, lon = LongitudeDegrees 5.726031 }

        ( mapModel, mapCmd ) =
            Map3d.init shared.layout.window origin
    in
    ( { map3d = mapModel }
    , Effect.sendCmd <| Cmd.map Map3dMsg mapCmd
    )


type Msg
    = NoOp
    | FlightsSelected String
    | Map3dMsg Map3d.Msg


update : Msg -> Model -> ( Model, Effect Msg )
update msg model =
    case msg of
        NoOp ->
            ( model, Effect.none )

        FlightsSelected flightId ->
            ( model, Effect.none )

        Map3dMsg mapMsg ->
            let
                ( mapModel, mapCmd ) =
                    Map3d.update mapMsg model.map3d
            in
            ( { model | map3d = mapModel }, Effect.sendCmd <| Cmd.map Map3dMsg mapCmd )


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ Sub.map Map3dMsg <| Map3d.subscriptions model.map3d
        ]


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
            [ Element.html <|
                Map3d.view
                    { restartOnboarding = NoOp
                    , flightSelected = FlightsSelected
                    , mapMsg = Map3dMsg
                    }
                    []
                    model.map3d
            ]
    }
