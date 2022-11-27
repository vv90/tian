port module Main exposing (..)

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
import Common.TaskProgressUtils exposing (progressPointsToMapItems, targetToMapItem)
import Components.Player as Player
import Element exposing (Element, column, layout, padding, spacing, text)
import Element.Font as Font
import Element.Input as Input
import Flags exposing (..)
import Html exposing (Html, button, div)
import Html.Attributes exposing (style)
import Http
import Json.Decode as D
import List.Extra as ListX
import List.Nonempty as NE exposing (Nonempty(..))
import Map as Map exposing (..)
import MapUtils exposing (..)
import Maybe.Extra as MaybeX
import Page.Demo as Demo
import Page.FlightTask.FlightTaskForm as FlightTaskForm
import Page.FlightTask.FlightTaskList as FlightTaskList
import Page.FlightTaskPage as FlightTaskPage
import Page.FlightTrack.FlightTrackUpload as FlightTrackUpload
import Page.Test.TestProgress as TestProgress
import Result.Extra as ResultX



-- port startDemo : () -> Cmd msg


port messageReceiver : (String -> msg) -> Sub msg


getDemoTaskCmd : Cmd Msg
getDemoTaskCmd =
    Http.request
        { method = "GET"
        , headers = []
        , url = "http://0.0.0.0:8081/demoTask"
        , body = Http.emptyBody
        , expect = Http.expectJson GotDemoFlightTask flightTaskDecoder
        , timeout = Nothing
        , tracker = Nothing
        }


startDemoCmd : Cmd Msg
startDemoCmd =
    Http.request
        { method = "GET"
        , headers = []
        , url = "http://0.0.0.0:8081/startDemo"
        , body = Http.emptyBody
        , expect = Http.expectWhatever (\_ -> NoMsg)
        , timeout = Nothing
        , tracker = Nothing
        }


main : Program Flags Model Msg
main =
    Browser.element
        { init = init
        , update = update
        , view = view
        , subscriptions = subscriptions
        }


type alias Model =
    { mapModel : Map.Model
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
                flags.windowSize
                9
                ( LatitudeDegrees 52.030558, LongitudeDegrees 39.662962 )
    in
    ( { mapModel = mapModel
      , flightTaskPage = FlightTaskPage.init

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
        ]
    )


type Msg
    = MapMsg Map.Msg
    | FlightTaskPageMsg FlightTaskPage.Msg
      -- | FlightTrackPageMsg FlightTrackUpload.Msg
    | TestProgressMsg TestProgress.Msg
    | AppStateMsg AppState.Msg
    | MessageReceived String
    | GotDemoFlightTask (ApiResult FlightTask)
    | DemoInitiated
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
        GotDemoFlightTask (Ok task) ->
            ( { model | flightTaskPage = FlightTaskPage.DemoPage (Demo.init task) }
              -- , startDemo ()
            , startDemoCmd
            )

        GotDemoFlightTask (Err _) ->
            ( model, Cmd.none )

        DemoInitiated ->
            ( model, getDemoTaskCmd )

        NoMsg ->
            ( model, Cmd.none )


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ Sub.map MapMsg (Map.subscriptions model.mapModel)
        , Sub.map FlightTaskPageMsg (FlightTaskPage.subscriptions model.flightTaskPage)
        , messageReceiver MessageReceived
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
            , style "background" "white"
            , style "border" "1px solid gray"
            , style "border-radius" "10px"
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
            [ Font.size 16 ]
            content
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
    in
    div []
        [ Map.view mapItems model.mapModel |> Html.map MapMsg
        , detachedView TopLeft <|
            Element.map FlightTaskPageMsg <|
                FlightTaskPage.view
                    { navPoints = model.appState.navPoints
                    , flightTasks = model.appState.flightTasks
                    }
                    model.flightTaskPage
        , detachedView TopRight <|
            column [ padding 10, spacing 10 ]
                (List.map text model.messages
                    ++ [ Input.button [] { onPress = Just DemoInitiated, label = text "Start Demo" } ]
                )

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
