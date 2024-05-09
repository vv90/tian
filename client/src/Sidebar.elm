module Sidebar exposing (MainCmds, Model(..), Msg(..), OnboardingStep(..), Props, init, sidebarWidth, update, view)

import Api.Types exposing (DeviceInfo, FlightInformation, FlightPosition, GeoPoint)
import Common.Deferred exposing (Deferred, deferredToMaybe)
import Common.Palette as Palette
import Components.Button exposing (primaryButton)
import Components.Map3d exposing (DemoType(..))
import Dict exposing (Dict)
import Element exposing (..)
import Element.Border as Border
import Element.Font as Font
import Element.Input as Input
import Flags exposing (Flags)
import Html exposing (Html, div)
import Html.Attributes exposing (style)
import List.Extra
import Ports
import VitePluginHelper


sidebarWidth : Int
sidebarWidth =
    480


type Model
    = GeneralUsage
    | OnboardingFlow { currentStep : OnboardingStep }


init : Flags -> Model
init flags =
    if flags.onboardingCompleted then
        GeneralUsage

    else
        OnboardingFlow { currentStep = WhatIsTian }


type OnboardingStep
    = WhatIsTian
    | FlightsTable
    | Controls ControlsStep
    | CameraMovementDemo


type ControlsStep
    = ControlsStepDrag
    | ControlsStepRotate
    | ControlsStepZoom


type alias MainCmds msg =
    { startDemo : DemoType -> Cmd msg
    , resetMap : Cmd msg
    , none : Cmd msg
    }


nexOnboaringStep : MainCmds msg -> OnboardingStep -> ( OnboardingStep, Cmd msg )
nexOnboaringStep mainCmds step =
    case step of
        WhatIsTian ->
            ( FlightsTable, mainCmds.none )

        FlightsTable ->
            ( Controls ControlsStepDrag, mainCmds.startDemo DragDemo )

        Controls ControlsStepDrag ->
            ( Controls ControlsStepRotate, mainCmds.startDemo RotateDemo )

        Controls ControlsStepRotate ->
            ( Controls ControlsStepZoom, mainCmds.startDemo ZoomDemo )

        Controls ControlsStepZoom ->
            ( CameraMovementDemo, mainCmds.startDemo ComplexDemo )

        CameraMovementDemo ->
            ( CameraMovementDemo, mainCmds.none )


type Msg
    = NextOnboardingStepRequested
    | OnboardingFinished
    | OnboardingRestarted


update : MainCmds msg -> Msg -> Model -> ( Model, Cmd Msg, Cmd msg )
update mainCmds msg state =
    case msg of
        NextOnboardingStepRequested ->
            case state of
                OnboardingFlow { currentStep } ->
                    let
                        ( nextStep, mainCmd ) =
                            nexOnboaringStep mainCmds currentStep
                    in
                    ( OnboardingFlow { currentStep = nextStep }, Cmd.none, mainCmd )

                _ ->
                    ( state, Cmd.none, mainCmds.none )

        OnboardingFinished ->
            ( GeneralUsage, Ports.onboardingCompleted (), mainCmds.resetMap )

        OnboardingRestarted ->
            ( OnboardingFlow { currentStep = WhatIsTian }, Cmd.none, mainCmds.none )


type alias Props msg =
    { flightPositions : Dict String ( Deferred FlightInformation, FlightPosition )
    , onFlightSelected : GeoPoint -> msg
    , mapMsg : Msg -> msg
    }


view : Props msg -> Model -> Html msg
view props state =
    div
        [ style "width" (String.fromInt sidebarWidth ++ "px")
        , style "background" "white"
        ]
        [ Element.layout
            [ Font.size 16
            , Font.family [ Font.typeface "Roboto" ]
            , paddingEach { top = 30, bottom = 10, left = 30, right = 30 }
            ]
            (column [ width fill, height fill, padding 10, spacing 20 ]
                [ image [ width (px 107) ]
                    { src = VitePluginHelper.asset "/assets/images/logo.svg"
                    , description = "Tian"
                    }
                , case state of
                    GeneralUsage ->
                        viewGeneralUsage props

                    OnboardingFlow { currentStep } ->
                        viewOnboarding props currentStep
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


viewGeneralUsage : Props msg -> Element msg
viewGeneralUsage props =
    viewFlightsTable props


viewFlightsTable : Props msg -> Element msg
viewFlightsTable { flightPositions, onFlightSelected } =
    let
        showDeviceInfo : DeviceInfo -> String
        showDeviceInfo info =
            [ info.registration, info.competitionNumber, info.aircraftModel ]
                |> List.filterMap identity
                |> String.join " | "

        numActiveFlights : Int
        numActiveFlights =
            flightPositions |> Dict.size
    in
    column [ spacing 10, height fill, width fill ]
        [ paragraph [ Font.bold ] [ text <| "Active flights:" ++ String.fromInt numActiveFlights ]
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
            (flightPositions
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
                            , onPress = Just <| onFlightSelected { lat = lat, lon = lon }
                            }
                    )
            )
        ]


viewOnboarding : Props msg -> OnboardingStep -> Element msg
viewOnboarding props step =
    let
        nextStepButton : Element Msg
        nextStepButton =
            primaryButton
                [ alignBottom, alignRight ]
                { label = text "Next"
                , onPress = Just NextOnboardingStepRequested
                }
    in
    case step of
        WhatIsTian ->
            column [ width fill, height fill, spacing 20 ]
                [ image [ width fill ]
                    { src = VitePluginHelper.asset "/assets/images/glider-pic.jpg"
                    , description = "Glider"
                    }
                , paragraph [] [ text "Tian is a flight monitoring app. It allows you to explore and follow flights on a 3D map." ]
                , paragraph [] [ text "Made by a glider pilot for glider pilots." ]
                , paragraph [] [ text "This app is a technical preview, it provides 3D elevation data only for limited area." ]
                , Element.map props.mapMsg nextStepButton
                ]

        FlightsTable ->
            column [ width fill, height fill, spacing 20 ]
                [ paragraph [] [ text "This table shows all active flights. Click on any flight to see it on the map." ]
                , viewFlightsTable props
                , Element.map props.mapMsg nextStepButton
                ]

        Controls currentStep ->
            let
                colorByState : ControlsStep -> List (Attribute msg)
                colorByState =
                    stepStateAttrs << stateOf currentStep
            in
            column [ width fill, height fill, spacing 20 ]
                [ paragraph [] [ text "Here's how you can navigate the map:" ]
                , paragraph (colorByState ControlsStepDrag) [ text "Drag – move the map" ]
                , paragraph (colorByState ControlsStepRotate) [ text "Right click + drag – rotate the camera" ]
                , paragraph (colorByState ControlsStepZoom) [ text "Scroll – zoom" ]
                , Element.map props.mapMsg nextStepButton
                ]

        CameraMovementDemo ->
            column [ width fill, height fill, spacing 20 ]
                [ text "In the future Tian could allow complex movements like this"
                , Element.map props.mapMsg <|
                    primaryButton
                        [ alignBottom, alignRight ]
                        { label = text "Finish onboarding"
                        , onPress = Just OnboardingFinished
                        }
                ]


type StepState
    = Passed
    | Active
    | Pending


stepStateAttrs : StepState -> List (Attribute msg)
stepStateAttrs state =
    case state of
        Passed ->
            [ Font.color Palette.darkerGray ]

        Active ->
            [ Font.color Palette.primary ]

        Pending ->
            [ Font.color Palette.darkGray ]


orderOfSteps : List ControlsStep
orderOfSteps =
    [ ControlsStepDrag, ControlsStepRotate, ControlsStepZoom ]


stateOf : ControlsStep -> ControlsStep -> StepState
stateOf currentStep subjectStep =
    let
        currentStepIndex : Maybe Int
        currentStepIndex =
            List.Extra.elemIndex currentStep orderOfSteps

        subjectStepIndex : Maybe Int
        subjectStepIndex =
            List.Extra.elemIndex subjectStep orderOfSteps
    in
    case ( currentStepIndex, subjectStepIndex ) of
        ( Just current, Just subject ) ->
            if current == subject then
                Active

            else if current > subject then
                Passed

            else
                Pending

        _ ->
            Pending



-- controlsStyle : ControlsStep -> ControlsStep -> List (Attribute msg)
-- controlsStyle controlStep current =
--     if current == step then
--         [ Font.bold, Font.color Palette.primary ]
--     else
--         [ Font.color Palette.darkGray ]
