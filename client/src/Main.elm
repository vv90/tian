module Main exposing (..)

-- import Geo.GeoUtils exposing (..)
-- import Utils exposing (..)

import Api.Entity exposing (Entity)
import Api.FlightTask exposing (FlightTask, flightTaskDecoder)
import Api.Geo exposing (Latitude(..), Longitude(..))
import Api.NavPoint exposing (NavPoint, navPointDecoder)
import Api.TaskProgress exposing (progressPointDecoder)
import AppState
import Browser
import Common.ApiResult exposing (ApiResult)
import Common.Deferred exposing (AsyncOperationStatus(..), Deferred(..), deferredToMaybe)
import Common.Effect as Effect
import Common.FlightTaskUtils exposing (navPoints, taskToMapItems)
import Common.GeoUtils exposing (metersElevation)
import Common.JsonCodecsExtra exposing (tupleDecoder)
import Common.Palette as Palette
import Common.TaskProgressUtils exposing (progressPointsToMapItems, targetToMapItem)
import Components.Player as Player
import Element exposing (Element, alignBottom, column, el, fill, height, layout, link, onRight, padding, paddingEach, px, rgba255, row, spacing, text, width)
import Element.Background as Background
import Element.Font as Font
import Element.Input as Input
import Env exposing (apiUrl)
import Flags exposing (..)
import Html exposing (Html, button, div)
import Html.Attributes exposing (style)
import Http
import Json.Decode as D
import List.Extra as ListX
import List.Nonempty as NE exposing (Nonempty(..))
import Map as Map exposing (..)
import Map3d as Map3d exposing (..)
import MapUtils exposing (..)
import Maybe.Extra as MaybeX
import Page.Demo as Demo
import Page.FlightTask.FlightTaskForm as FlightTaskForm
import Page.FlightTask.FlightTaskList as FlightTaskList
import Page.FlightTaskPage as FlightTaskPage
import Page.FlightTrack.FlightTrackUpload as FlightTrackUpload
import Page.Test.TestProgress as TestProgress



-- getDemoTaskCmd : Cmd Msg
-- getDemoTaskCmd =
--     Http.request
--         { method = "GET"
--         , headers = []
--         , url = apiUrl "demoTask"
--         , body = Http.emptyBody
--         , expect = Http.expectJson (Finished >> DemoInit) flightTaskDecoder
--         , timeout = Nothing
--         , tracker = Nothing
--         }
-- startDemoCmd : Cmd Msg
-- startDemoCmd =
--     Http.request
--         { method = "GET"
--         , headers = []
--         , url = apiUrl "startDemo"
--         , body = Http.emptyBody
--         , expect = Http.expectWhatever (\_ -> NoMsg)
--         , timeout = Nothing
--         , tracker = Nothing
--         }


main : Program Flags Model Msg
main =
    Browser.element
        { init = init
        , update = update
        , view = view
        , subscriptions = subscriptions
        }


sidebarWidth =
    480


withSidebarOffset : WindowSize -> WindowSize
withSidebarOffset windowSize =
    { height = windowSize.height
    , width = windowSize.width - sidebarWidth
    }


type alias Model =
    { mapModel : Map.Model
    , map3dModel : Map3d.Model
    , flightTaskPage : FlightTaskPage.Model

    -- , flightTrackPage : Maybe FlightTrackUpload.Model
    , testProgressModel : TestProgress.Model
    , appState : AppState.Model
    , messages : List String
    , demoTask : Maybe FlightTask
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
        mapModel =
            Map.init
                (withSidebarOffset flags.windowSize)
                9
                ( LatitudeDegrees 52.030558, LongitudeDegrees 39.662962 )

        ( map3dModel, m3dCmd ) =
            Map3d.init
                (withSidebarOffset flags.windowSize)
                ( LatitudeDegrees 52.030558, LongitudeDegrees 39.662962 )
    in
    ( { mapModel = mapModel
      , map3dModel = map3dModel
      , flightTaskPage = FlightTaskPage.DemoPage Demo.init

      --   , flightTrackPage = Nothing
      , testProgressModel = TestProgress.init
      , appState =
            AppState.init
                |> AppState.withPendingNavPoints
                |> AppState.withPendingFlightTasks
      , messages = []
      , demoTask = Nothing
      }
    , Cmd.batch
        [ Cmd.map AppStateMsg AppState.getNavPointsCmd
        , Cmd.map AppStateMsg AppState.getFlightTasksCmd
        , Cmd.map Map3dMsg m3dCmd
        ]
    )


type Msg
    = MapMsg Map.Msg
    | Map3dMsg Map3d.Msg
    | FlightTaskPageMsg FlightTaskPage.Msg
      -- | FlightTrackPageMsg FlightTrackUpload.Msg
    | TestProgressMsg TestProgress.Msg
    | AppStateMsg AppState.Msg
    | MessageReceived String
      -- | GotDemoFlightTask (ApiResult FlightTask)
      -- | DemoInit (AsyncOperationStatus (ApiResult FlightTask))
    | NoMsg


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        MapMsg m ->
            let
                ( nextModel, cmd, point ) =
                    Map.update m model.mapModel

                selectedTaskId =
                    case model.flightTaskPage of
                        FlightTaskPage.UploadTrack utm ->
                            Just utm.taskId

                        _ ->
                            Nothing

                -- for debugging
                selectedPoints =
                    case model.testProgressModel.progress of
                        NotStarted ->
                            Just []

                        InProgress ->
                            Nothing

                        Updating _ ->
                            Nothing

                        Resolved pts ->
                            Result.toMaybe pts
                                |> Maybe.map (.points >> List.map (\p -> ( p.lat, p.lon )))

                -- for debugging
                appendPoint ps p =
                    MaybeX.unwrap
                        (NE.fromElement p)
                        (\xs -> NE.append xs (NE.fromElement p))
                        (NE.fromList ps)
            in
            ( { model | mapModel = nextModel }
            , Cmd.batch
                [ Cmd.map MapMsg cmd

                -- for debugging
                -- , Maybe.map3
                --     (\p ps tid ->
                --         Cmd.map TestProgressMsg <|
                --             TestProgress.updateProgressCmd
                --                 tid
                --                 (appendPoint ps p)
                --     )
                --     point
                --     selectedPoints
                --     selectedTaskId
                --     |> Maybe.withDefault Cmd.none
                ]
            )

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

                applyEffect e =
                    case e of
                        FlightTaskPage.FlightTaskSaved _ ->
                            ( \m -> m |> withAppState (m.appState |> AppState.withPendingFlightTasks)
                            , Cmd.map AppStateMsg AppState.getFlightTasksCmd
                            )
            in
            ( model |> withFlightTaskPage nextModel, Cmd.map FlightTaskPageMsg cmd )
                |> Effect.applyAll applyEffect effs

        -- FlightTrackPageMsg ftPageMsg ->
        --     case model.flightTrackPage of
        --         Just ftpModel ->
        --             let
        --                 ( nextModel, cmd ) =
        --                     FlightTrackUpload.update ftPageMsg ftpModel
        --             in
        --             ( { model | flightTrackPage = Just nextModel }
        --             , Cmd.map FlightTrackPageMsg cmd
        --             )
        --         Nothing ->
        --             ( model, Cmd.none )
        TestProgressMsg tpMsg ->
            let
                ( nextModel, cmd ) =
                    TestProgress.update tpMsg model.testProgressModel
            in
            ( { model | testProgressModel = nextModel }, Cmd.map TestProgressMsg cmd )

        AppStateMsg m ->
            let
                ( nextModel, cmd ) =
                    AppState.update m model.appState
            in
            ( { model | appState = nextModel }, Cmd.map AppStateMsg cmd )

        MessageReceived str ->
            let
                upd =
                    D.decodeString (tupleDecoder ( D.string, progressPointDecoder )) str
            in
            case ( upd, model.flightTaskPage ) of
                ( Ok ( id, p ), FlightTaskPage.DemoPage pm ) ->
                    ( { model
                        | flightTaskPage =
                            FlightTaskPage.DemoPage (pm |> Demo.withPointUpdate id p)
                      }
                    , Cmd.none
                    )

                _ ->
                    ( model, Cmd.none )

        -- ( { model | messages = str :: model.messages }, Cmd.none )
        -- GotDemoFlightTask (Ok task) ->
        --     ( { model | flightTaskPage = FlightTaskPage.DemoPage (Demo.init task) }
        --       , startDemo ()
        --     )
        -- GotDemoFlightTask (Err _) ->
        --     ( model, Cmd.none )
        -- DemoInitiated ->
        --     ( model, startDemoCmd )
        -- DemoInit Started ->
        --     ( model, getDemoTaskCmd )
        -- DemoInit (Finished (Ok task)) ->
        --     ( { model | flightTaskPage = FlightTaskPage.DemoPage  }
        --     , startDemo ()
        --     )
        -- DemoInit (Finished (Err e)) ->
        --     ( model, Cmd.none )
        NoMsg ->
            ( model, Cmd.none )


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ Sub.map MapMsg (Map.subscriptions model.mapModel)
        , Sub.map Map3dMsg (Map3d.subscriptions model.map3dModel)
        , Sub.map FlightTaskPageMsg (FlightTaskPage.subscriptions model.flightTaskPage)

        -- , messageReceiver MessageReceived
        ]


type DetachedPosition
    = TopLeft
    | TopRight
    | BottomLeft
    | BottomRight


detachedView : DetachedPosition -> Element msg -> Html msg
detachedView pos content =
    let
        viewStyle =
            [ style "position" "absolute"
            , style "margin" "10px"
            , style "padding" "10px"
            , style "background" "rgba(255, 255, 255, 0.8)"
            , style "border" "1px solid rgba(37, 37, 37, 0.5)"
            , style "border-radius" "5px"
            , style "color" "#252525"
            , style "font-family" "Roboto"
            ]

        positionStyle =
            case pos of
                TopLeft ->
                    [ style "top" "0", style "left" "0" ]

                TopRight ->
                    [ style "top" "0", style "right" "0" ]

                BottomLeft ->
                    [ style "bottom" "0", style "left" "0" ]

                BottomRight ->
                    [ style "bottom" "0", style "right" "0" ]
    in
    div
        (viewStyle ++ positionStyle)
        [ Element.layout
            [ Font.size 16
            , Font.family [ Font.typeface "Roboto" ]
            ]
            content
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


selectedFlightTask : Deferred (ApiResult (List (Entity Int FlightTask))) -> Int -> Maybe (Entity Int FlightTask)
selectedFlightTask flightTasksD taskId =
    case flightTasksD of
        Resolved (Ok items) ->
            ListX.find (\{ key } -> key == taskId) items

        _ ->
            Nothing


view : Model -> Html Msg
view model =
    let
        mapItems =
            case model.flightTaskPage of
                FlightTaskPage.AddTask pm ->
                    FlightTaskForm.mapItems pm

                FlightTaskPage.UploadTrack pm ->
                    -- taskItems ++ pointItems ++ TestProgress.toMapItems model.testProgressModel
                    FlightTrackUpload.mapItems (AppState.resolvedTasks model.appState) pm

                FlightTaskPage.SelectTask ->
                    []

                FlightTaskPage.DemoPage pm ->
                    Demo.mapItems pm

        map3dItems =
            case model.flightTaskPage of
                FlightTaskPage.UploadTrack pm ->
                    FlightTrackUpload.map3dItems (AppState.resolvedTasks model.appState) pm

                FlightTaskPage.DemoPage pm ->
                    Demo.map3dItems pm

                _ ->
                    []
    in
    div
        [ style "display" "flex"
        , style "flex-direction" "row"
        ]
        -- [ Map.view mapItems model.mapModel |> Html.map MapMsg
        [ sidebar <|
            Element.map FlightTaskPageMsg <|
                FlightTaskPage.view
                    { navPoints = model.appState.navPoints
                    , flightTasks = model.appState.flightTasks
                    }
                    model.flightTaskPage
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
