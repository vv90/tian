module Main exposing
    ( Model
    , Msg(..)
    , main
    )

import Api.Types exposing (..)
import AppState
import Axis3d
import Browser
import Camera3d
import Common.Effect as Effect
import Common.JsonCodecsExtra exposing (tupleDecoder)
import Common.Palette as Palette
import Components.Map3d as Map3d exposing (camera, screenRectangle)
import Components.Map3dUtils exposing (Map3dItem(..), MercatorCoords, MercatorUnit, containingTile)
import Demo.Demo as Demo
import Demo.FlightTaskPage as FlightTaskPage
import Demo.Test.TestProgress as TestProgress
import Dict exposing (Dict)
import Element exposing (..)
import Element.Font as Font
import Flags exposing (Flags, WindowSize)
import Html exposing (Html, div)
import Html.Attributes exposing (style)
import Json.Decode as D
import Plane3d
import Point2d exposing (Point2d)
import Point3d
import Ports exposing (flightPositionReceiver, watchFlight)
import SketchPlane3d
import Tile exposing (TileKey, ZoomLevel(..))


main : Program Flags Model Msg
main =
    Browser.element
        { init = init
        , update = update
        , view = view
        , subscriptions = subscriptions
        }


sidebarWidth : Int
sidebarWidth =
    480


withSidebarOffset : WindowSize -> WindowSize
withSidebarOffset windowSize =
    { height = windowSize.height
    , width = windowSize.width - sidebarWidth
    }


type alias Model =
    { map3dModel : Map3d.Model
    , flightTaskPage : FlightTaskPage.Model

    -- , flightTrackPage : Maybe FlightTrackUpload.Model
    , testProgressModel : TestProgress.Model
    , appState : AppState.Model
    , messages : List String
    , demoTask : Maybe FlightTask
    , flightPositions : Dict String ( GeoPoint, Elevation )
    }


withAppState : AppState.Model -> Model -> Model
withAppState appState model =
    { model | appState = appState }


withFlightTaskPage : FlightTaskPage.Model -> Model -> Model
withFlightTaskPage ftpModel model =
    { model | flightTaskPage = ftpModel }


init : Flags -> ( Model, Cmd Msg )
init flags =
    let
        ( map3dModel, m3dCmd ) =
            Map3d.init
                (withSidebarOffset flags.windowSize)
                -- ( LatitudeDegrees 52.030558, LongitudeDegrees 39.662962 )
                { lat = LatitudeDegrees 45.208451, lon = LongitudeDegrees 5.726031 }
    in
    ( { map3dModel = map3dModel
      , flightTaskPage = FlightTaskPage.DemoPage Demo.init

      --   , flightTrackPage = Nothing
      , testProgressModel = TestProgress.init
      , appState =
            AppState.init
                |> AppState.withPendingNavPoints
                |> AppState.withPendingFlightTasks
      , messages = []
      , demoTask = Nothing
      , flightPositions = Dict.empty
      }
    , Cmd.batch
        [ Cmd.map AppStateMsg AppState.getNavPointsCmd
        , Cmd.map AppStateMsg AppState.getFlightTasksCmd
        , Cmd.map Map3dMsg m3dCmd
        , watchFlight ()
        ]
    )


type Msg
    = Map3dMsg Map3d.Msg
    | FlightTaskPageMsg FlightTaskPage.Msg
    | AppStateMsg AppState.Msg
      -- | GotDemoFlightTask (ApiResult FlightTask)
      -- | DemoInit (AsyncOperationStatus (ApiResult FlightTask))
    | FlightPositionReceived String


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Map3dMsg m3dMsg ->
            let
                ( nextModel, m3dCmd ) =
                    Map3d.update m3dMsg model.map3dModel
            in
            ( { model | map3dModel = nextModel }
            , Cmd.map Map3dMsg m3dCmd
            )

        FlightTaskPageMsg ftPageMsg ->
            let
                ( nextModel, cmd, effs ) =
                    FlightTaskPage.update ftPageMsg model.flightTaskPage

                applyEffect : FlightTaskPage.Effect -> ( Model -> Model, Cmd Msg )
                applyEffect e =
                    case e of
                        FlightTaskPage.FlightTaskSaved ->
                            ( \m -> m |> withAppState (m.appState |> AppState.withPendingFlightTasks)
                            , Cmd.map AppStateMsg AppState.getFlightTasksCmd
                            )
            in
            ( model |> withFlightTaskPage nextModel, Cmd.map FlightTaskPageMsg cmd )
                |> Effect.applyAll applyEffect effs

        AppStateMsg m ->
            let
                ( nextModel, cmd ) =
                    AppState.update m model.appState
            in
            ( { model | appState = nextModel }, Cmd.map AppStateMsg cmd )

        FlightPositionReceived str ->
            let
                pos : Result D.Error ( String, ( GeoPoint, Elevation ) )
                pos =
                    D.decodeString (tupleDecoder ( D.string, tupleDecoder ( geoPointDecoder, elevationDecoder ) )) str
            in
            case pos of
                Ok ( key, val ) ->
                    ( { model | flightPositions = Dict.insert key val model.flightPositions }, Cmd.none )

                Err _ ->
                    ( model, Cmd.none )


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ Sub.map Map3dMsg (Map3d.subscriptions model.map3dModel)
        , Sub.map FlightTaskPageMsg (FlightTaskPage.subscriptions model.flightTaskPage)
        , flightPositionReceiver FlightPositionReceived

        -- , messageReceiver MessageReceived
        ]


sidebar : Element msg -> Html msg
sidebar content =
    div
        [ style "width" (String.fromInt sidebarWidth ++ "px")
        , style "background" "white"
        ]
        [ Element.layout
            [ Font.size 16
            , Font.family [ Font.typeface "Roboto" ]
            , paddingEach { top = 30, bottom = 10, left = 30, right = 30 }
            ]
            (column [ height fill ]
                [ content
                , column
                    [ alignBottom
                    , Font.color Palette.darkGray
                    , spacing 5
                    , Font.size 14
                    ]
                    [ text "Contact me: "
                    , row []
                        [ text " • "
                        , link [ Font.color Palette.primary ]
                            { url = "mailto:vladimir.kirienko.e@gmail.com"
                            , label = text "vladimir.kirienko.e@gmail.com"
                            }
                        ]
                    , row []
                        [ text " • "
                        , link [ Font.color Palette.primary ]
                            { url = "https://www.linkedin.com/in/vladimir-kirienko/"
                            , label = text "linkedin.com/in/vladimir-kirienko"
                            }
                        ]
                    ]
                ]
            )
        ]


view : Model -> Html Msg
view model =
    let
        map3dItems : List Map3dItem
        map3dItems =
            model.flightPositions |> Dict.toList |> List.map (\( key, ( pt, elev ) ) -> Marker key pt elev)

        -- case model.flightTaskPage of
        --     FlightTaskPage.UploadTrack pm ->
        --         FlightTrackUpload.map3dItems (AppState.resolvedTasks model.appState) pm
        --     FlightTaskPage.DemoPage pm ->
        --         Demo.map3dItems pm
        --     _ ->
        --         []
        projectCursor : ( Float, Float ) -> Maybe (Point2d MercatorUnit MercatorCoords)
        projectCursor ( x, y ) =
            Camera3d.ray
                (camera model.map3dModel.viewArgs)
                (screenRectangle model.map3dModel.windowSize)
                (Point2d.pixels x (toFloat model.map3dModel.windowSize.height - y))
                |> Axis3d.intersectionWithPlane Plane3d.xy
                |> Maybe.map (Point3d.at_ model.map3dModel.mercatorRate >> Point3d.projectInto SketchPlane3d.xy >> Point2d.relativeTo model.map3dModel.mapFrame)

        cursorPosition : Maybe TileKey
        cursorPosition =
            model.map3dModel.cursorPosition
                |> Maybe.andThen projectCursor
                |> Maybe.map (containingTile Z12)

        showTileKey : TileKey -> String
        showTileKey ( x, y, zoom ) =
            String.fromInt x ++ ", " ++ String.fromInt y ++ ", " ++ String.fromInt zoom
    in
    div
        [ style "display" "flex"
        , style "flex-direction" "row"
        ]
        -- [ Map.view mapItems model.mapModel |> Html.map MapMsg
        [ cursorPosition
            |> Maybe.map (showTileKey >> text)
            |> Maybe.withDefault (text "No cursor")
            |> sidebar

        -- Element.map FlightTaskPageMsg <|
        --     FlightTaskPage.view
        --         { navPoints = model.appState.navPoints
        --         , flightTasks = model.appState.flightTasks
        --         }
        --         model.flightTaskPage
        , Map3d.view map3dItems model.map3dModel |> Html.map Map3dMsg

        -- , detachedView TopLeft <|
        --     Element.map FlightTaskPageMsg <|
        --         FlightTaskPage.view
        --             { navPoints = model.appState.navPoints
        --             , flightTasks = model.appState.flightTasks
        --             }
        --             model.flightTaskPage
        -- , detachedView TopRight <|
        --     column [ padding 10, spacing 10 ]
        --         (List.map text model.messages
        --             ++ [ Input.button [] { onPress = Just <| DemoInit Started, label = text "Start Demo" } ]
        --         )
        -- , detachedView BottomLeft <|
        --     column [ padding 10, spacing 10 ]
        --         (List.map text (Map3d.debugInfo model.map3dModel))
        -- for debugging
        -- , detachedView TopRight <|
        --     Element.map TestProgressMsg <|
        --         TestProgress.view model.testProgressModel
        -- , MaybeX.unwrap
        --     (div [] [])
        --     (\(taskId, ftpModel) ->
        --         detachedView TopRight <|
        --             Element.map FlightTrackPageMsg <|
        --                 FlightTrackUpload.view ftpModel
        --     )
        --     model.flightTrackPage
        -- , detachedView TopRight <|
        --     Element.text "Hello"
        ]
