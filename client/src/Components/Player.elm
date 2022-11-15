module Components.Player exposing (..)

import Api.TaskProgress exposing (ProgressPoint, TaskProgress)
import Common.GeoUtils exposing (degreesLatitude, degreesLongitude, metersDistance)
import Common.Utils exposing (roundN)
import Components.PlaybackSpeed exposing (PlaybackSpeed(..), increaseSpeed, lowerSpeed, playbackCoefficient)
import Element exposing (Element, column, none, padding, row, spacing, text)
import Element.Input as Input
import List.Extra as ListX
import Maybe.Extra as MaybeX
import Time exposing (Posix, millisToPosix, posixToMillis)
import TimeUtils exposing (formatTime)


type alias Model =
    { speed : PlaybackSpeed
    , playing : Bool
    , startTime : Posix
    , endTime : Posix
    , currentTime : Posix
    , trackPoints : List ProgressPoint
    , currentPoint : Maybe ProgressPoint
    }


init : TaskProgress -> Maybe Model
init { points } =
    let
        firstPoint =
            List.head points

        lastPoint =
            ListX.last points
    in
    Maybe.map2
        (\first last ->
            { speed = X1
            , playing = False
            , startTime = millisToPosix first.time
            , endTime = millisToPosix last.time
            , currentTime = millisToPosix first.time
            , trackPoints = points
            , currentPoint = firstPoint
            }
        )
        firstPoint
        lastPoint


paused : Model -> Model
paused model =
    { model | playing = False }


started : Model -> Model
started model =
    { model | playing = True }


withSpeed : PlaybackSpeed -> Model -> Model
withSpeed speed model =
    { model | speed = speed }


withCurrentPoint : Maybe ProgressPoint -> Model -> Model
withCurrentPoint currentPoint model =
    { model | currentPoint = currentPoint }


advancePlayback : Int -> Model -> Model
advancePlayback millis model =
    let
        newTimeMillis =
            posixToMillis model.currentTime + millis

        -- 100 * playbackCoefficient model.speed
    in
    case model.trackPoints of
        [] ->
            { model | trackPoints = [], currentTime = millisToPosix newTimeMillis }

        p :: ps ->
            if newTimeMillis >= p.time then
                advancePlayback
                    0
                    { model
                        | trackPoints = ps
                        , currentTime = millisToPosix newTimeMillis
                        , currentPoint = Just p
                    }

            else
                { model | currentTime = millisToPosix newTimeMillis }


type Msg
    = Started
    | Paused
    | SpeedChanged PlaybackSpeed
    | PlaybackAdvanced Int


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Started ->
            ( started model, Cmd.none )

        Paused ->
            ( paused model, Cmd.none )

        SpeedChanged speed ->
            ( withSpeed speed model, Cmd.none )

        PlaybackAdvanced millis ->
            ( advancePlayback millis model, Cmd.none )


subscriptions : Model -> Sub Msg
subscriptions model =
    if model.playing then
        Time.every 100 (always <| PlaybackAdvanced (100 * playbackCoefficient model.speed))

    else
        Sub.none


view : Model -> Element Msg
view model =
    let
        playbackButton =
            if model.playing then
                Input.button
                    []
                    { onPress = Just Paused
                    , label = text "Pause"
                    }

            else
                Input.button
                    []
                    { onPress = Just Started
                    , label = text "Play"
                    }

        lowerSpeedMsg =
            Maybe.map SpeedChanged <| lowerSpeed model.speed

        increaseSpeedMsg =
            Maybe.map SpeedChanged <| increaseSpeed model.speed

        lowerSpeedButton =
            Input.button
                []
                { onPress = lowerSpeedMsg
                , label = text "<"
                }

        increaseSpeedButton =
            Input.button
                []
                { onPress = increaseSpeedMsg
                , label = text ">"
                }

        stats =
            MaybeX.unwrap
                []
                (\p ->
                    [ p.distance |> (\d -> roundN 2 (d / 1000)) |> String.fromFloat |> text
                    , p.speed |> MaybeX.unwrap none ((\s -> roundN 2 (s * 3.6)) >> String.fromFloat >> text)
                    , p.target |> MaybeX.unwrap none text
                    ]
                )
                model.currentPoint
    in
    column
        [ spacing 10, padding 10 ]
        [ row
            [ spacing 10 ]
            [ text <|
                if model.playing then
                    "Playing"

                else
                    "Paused"
            , text <| (String.fromInt >> (++) "X") <| playbackCoefficient model.speed
            , text <| formatTime model.currentTime
            , text <| Maybe.withDefault "" <| Maybe.map (.time >> String.fromInt) model.currentPoint
            ]

        -- , row
        --     [ spacing 10 ]
        --     [ text <|
        --         MaybeX.unwrap
        --             "-"
        --             (.lat >> degreesLatitude >> String.fromFloat)
        --             model.currentPoint
        --     , text <|
        --         MaybeX.unwrap
        --             "-"
        --             (.lon >> degreesLongitude >> String.fromFloat)
        --             model.currentPoint
        --     ]
        -- , row
        --     [ spacing 10 ]
        --     [ text <| String.fromInt <| posixToMillis model.currentTime
        --     , text <| MaybeX.unwrap "-" (.time >> String.fromInt) model.currentPoint
        --     ]
        , row
            [ spacing 10 ]
            [ lowerSpeedButton, playbackButton, increaseSpeedButton ]
        , row
            [ spacing 10 ]
            stats
        ]
