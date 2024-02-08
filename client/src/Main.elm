module Main exposing
    ( Model
    , Msg(..)
    , main
    )

import Api.Types exposing (..)
import AppState
import Browser
import Common.ApiCommands exposing (getCurrentFlights, getFlightInformation)
import Common.ApiResult exposing (ApiResult)
import Common.Deferred exposing (Deferred(..), deferredToMaybe)
import Common.Effect as Effect
import Common.JsonCodecsExtra exposing (tupleDecoder)
import Common.Palette as Palette
import Components.Map3d as Map3d
import Components.Map3dUtils exposing (Map3dItem(..))
import Demo.Demo as Demo
import Demo.FlightTaskPage as FlightTaskPage
import Demo.Test.TestProgress as TestProgress
import Dict exposing (Dict)
import Element exposing (..)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Element.Input as Input
import Flags exposing (Flags, WindowSize)
import Html exposing (Html, div)
import Html.Attributes exposing (style)
import Json.Decode as D
import Ports exposing (flightPositionReceiver, watchFlight)


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



-- type DeviceInfoData
--     = NotAsked
--     | NoData
--     | DeviceInfoData DeviceInfo


type alias Model =
    { map3dModel : Map3d.Model
    , flightTaskPage : FlightTaskPage.Model

    -- , flightTrackPage : Maybe FlightTrackUpload.Model
    , testProgressModel : TestProgress.Model
    , appState : AppState.Model
    , messages : List String

    -- , demoTask : Maybe FlightTask
    , flightPositions : Dict String ( Deferred FlightInformation, FlightPosition )
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

      --   , demoTask = Nothing
      , flightPositions = Dict.empty
      }
    , Cmd.batch
        [ Cmd.map Map3dMsg m3dCmd
        , watchFlight ()
        , getCurrentFlights CurrentFlightsReceived
        ]
    )


type Msg
    = Map3dMsg Map3d.Msg
    | FlightTaskPageMsg FlightTaskPage.Msg
    | AppStateMsg AppState.Msg
      -- | GotDemoFlightTask (ApiResult FlightTask)
      -- | DemoInit (AsyncOperationStatus (ApiResult FlightTask))
    | FlightPositionReceived String
    | FlightInformationReceived String (ApiResult (Maybe FlightInformation))
    | CurrentFlightsReceived (ApiResult (List ( String, ( FlightInformation, FlightPosition ) )))


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
                pos : Result D.Error ( DeviceId, FlightPosition )
                pos =
                    D.decodeString (tupleDecoder ( deviceIdDecoder, flightPositionDecoder )) str

                updateFlights : DeviceId -> FlightPosition -> ( Model, Cmd Msg )
                updateFlights (DeviceId key) position =
                    case Dict.get key model.flightPositions of
                        Just ( NotStarted, _ ) ->
                            ( { model | flightPositions = Dict.insert key ( InProgress, position ) model.flightPositions }
                            , getFlightInformation key (FlightInformationReceived key)
                            )

                        Just ( InProgress, _ ) ->
                            ( { model | flightPositions = Dict.insert key ( InProgress, position ) model.flightPositions }
                            , Cmd.none
                            )

                        Just ( Updating info, _ ) ->
                            ( { model | flightPositions = Dict.insert key ( Updating info, position ) model.flightPositions }
                            , Cmd.none
                            )

                        Just ( Resolved info, _ ) ->
                            ( { model | flightPositions = Dict.insert key ( Resolved info, position ) model.flightPositions }
                            , Cmd.none
                            )

                        Nothing ->
                            ( { model | flightPositions = Dict.insert key ( InProgress, position ) model.flightPositions }
                            , getFlightInformation key (FlightInformationReceived key)
                            )
            in
            case pos of
                Ok ( key, val ) ->
                    updateFlights key val

                Err _ ->
                    ( model, Cmd.none )

        FlightInformationReceived key (Ok (Just info)) ->
            case Dict.get key model.flightPositions of
                Just ( _, position ) ->
                    ( { model | flightPositions = Dict.insert key ( Resolved info, position ) model.flightPositions }
                    , Cmd.none
                    )

                _ ->
                    ( model, Cmd.none )

        FlightInformationReceived _ _ ->
            ( model, Cmd.none )

        CurrentFlightsReceived (Ok flights) ->
            ( { model
                | flightPositions =
                    Dict.fromList flights
                        |> Dict.map (\_ -> Tuple.mapFirst Resolved)
              }
            , Cmd.none
            )

        CurrentFlightsReceived (Err _) ->
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
        primaryButton : { label : Element msg, onPress : Maybe msg } -> Element msg
        primaryButton { label, onPress } =
            Input.button
                [ Font.color Palette.white
                , Background.color Palette.primary
                , padding 10
                , Border.rounded 5
                ]
                { onPress = onPress, label = label }

        map3dItems : List Map3dItem
        map3dItems =
            let
                label : String -> Deferred FlightInformation -> String
                label key info =
                    info
                        |> deferredToMaybe
                        |> Maybe.andThen .deviceInfo
                        |> Maybe.andThen .competitionNumber
                        |> Maybe.withDefault key
            in
            model.flightPositions
                |> Dict.toList
                |> List.map (\( key, ( info, { lat, lon, alt } ) ) -> Marker (label key info) { lat = lat, lon = lon } alt)

        numActiveFlights : Int
        numActiveFlights =
            model.flightPositions |> Dict.size

        showDeviceInfo : DeviceInfo -> String
        showDeviceInfo info =
            [ info.registration, info.competitionNumber, info.aircraftModel ]
                |> List.filterMap identity
                |> String.join " | "

        tutorial : Element Msg
        tutorial =
            column [ padding 10, spacing 10, height fill, width fill ]
                [ paragraph [ Font.bold ] [ text "Controls" ]
                , paragraph [] [ text "Drag – move the map" ]
                , paragraph [] [ text "Right click + drag – rotate the camera" ]
                , paragraph [] [ text "Scroll – zoom" ]
                , primaryButton
                    { onPress =
                        case model.map3dModel.demoState of
                            Map3d.DemoNotStarted ->
                                Just <| Map3dMsg Map3d.DemoStarted

                            Map3d.DemoInProgress _ ->
                                Just <| Map3dMsg Map3d.DemoFinished
                    , label =
                        case model.map3dModel.demoState of
                            Map3d.DemoNotStarted ->
                                text "Start Demo"

                            Map3d.DemoInProgress _ ->
                                text "Stop Demo"
                    }
                , paragraph [ Font.bold ] [ text <| "Active flights:" ++ String.fromInt numActiveFlights ]
                , column
                    [ spacing 20
                    , padding 10
                    , scrollbarY
                    , height <| minimum 100 <| maximum 500 <| fill
                    , width <| minimum 100 <| fill
                    , Border.width 1
                    , Border.color Palette.lightGray
                    , Border.rounded 5
                    ]
                    (model.flightPositions
                        |> Dict.toList
                        |> List.map
                            (\( key, ( info, { lat, lon } ) ) ->
                                Input.button
                                    []
                                    { label =
                                        info
                                            |> deferredToMaybe
                                            |> Maybe.andThen .deviceInfo
                                            |> Maybe.map showDeviceInfo
                                            |> Maybe.map (\i -> key ++ " | " ++ i)
                                            |> Maybe.withDefault key
                                            |> text
                                    , onPress = Just <| Map3dMsg <| Map3d.PointFocused { lat = lat, lon = lon }
                                    }
                            )
                    )
                ]
    in
    div
        [ style "display" "flex"
        , style "flex-direction" "row"
        ]
        [ tutorial
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
