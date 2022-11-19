module Components.Player exposing (..)

import Api.TaskProgress exposing (ProgressPoint, TaskProgress)
import Common.GeoUtils exposing (GeoPoint, degreesLatitude, degreesLongitude, metersDistance, metersElevation)
import Common.Utils exposing (roundN)
import Components.PlaybackSpeed exposing (PlaybackSpeed(..), increaseSpeed, lowerSpeed, playbackCoefficient)
import Element exposing (Element, column, none, padding, row, spacing, text)
import Element.Input as Input
import Element.Lazy exposing (lazy)
import List.Extra as ListX
import Maybe.Extra as MaybeX
import Time exposing (Posix, millisToPosix, posixToMillis)
import TimeUtils exposing (formatTime)


type alias PlaybackState =
    { compId : String
    , past : List ProgressPoint
    , future : List ProgressPoint
    }


updatePlaybackState : Int -> PlaybackState -> PlaybackState
updatePlaybackState time state =
    case state.future of
        [] ->
            state

        p :: ps ->
            if time < p.time then
                state

            else
                updatePlaybackState
                    time
                    { state | past = p :: state.past, future = ps }


type alias Model =
    { speed : PlaybackSpeed
    , playing : Bool
    , startTime : Int
    , currentTime : Int
    , states : List PlaybackState
    }


init : List TaskProgress -> Maybe Model
init progressList =
    let
        initTime =
            progressList
                |> List.filterMap (.points >> List.head >> Maybe.map .time)
                |> List.minimum

        initState : TaskProgress -> PlaybackState
        initState p =
            { compId = p.compId
            , past = []
            , future = p.points
            }
    in
    Maybe.map
        (\t ->
            { speed = X1
            , playing = False
            , startTime = t
            , currentTime = t
            , states =
                progressList
                    |> List.map initState
            }
        )
        initTime


currPoints : Model -> List ( String, Float, GeoPoint )
currPoints model =
    let
        position : ProgressPoint -> GeoPoint
        position point =
            ( point.lat, point.lon )
    in
    model.states
        -- |> List.filterMap (\s -> List.head s.past |> Maybe.map (\p -> (s.compId, p.)) )
        |> List.filterMap (\s -> s.past |> List.head |> Maybe.map (\p -> ( s.compId, metersElevation p.altitude, position p )))


paused : Model -> Model
paused model =
    { model | playing = False }


started : Model -> Model
started model =
    { model | playing = True }


withSpeed : PlaybackSpeed -> Model -> Model
withSpeed speed model =
    { model | speed = speed }


advancePlayback : Int -> Model -> Model
advancePlayback millis model =
    let
        newTimeMillis =
            model.currentTime + millis
    in
    { model
        | currentTime = newTimeMillis
        , states = List.map (updatePlaybackState newTimeMillis) model.states
    }


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

        -- playbackStateView : PlaybackState -> List (Element Msg)
        toStats : PlaybackState -> Maybe ( Float, Float, String )
        toStats state =
            List.head state.past
                |> Maybe.andThen
                    (\p ->
                        Maybe.map2
                            (\s t ->
                                ( roundN 2 (s * 3.6)
                                , roundN 2 (p.distance / 1000)
                                , t
                                )
                            )
                            p.speed
                            p.target
                    )

        leaderboard states =
            states
                |> List.filterMap toStats
                |> List.sortBy (\( speed, _, _ ) -> speed)
                |> List.reverse
                |> List.map
                    (\( speed, distance, target ) ->
                        row
                            [ spacing 10 ]
                            [ text <| String.fromFloat speed ++ " km/h"
                            , text <| String.fromFloat distance ++ " km"
                            , text target
                            ]
                    )
                |> column [ spacing 10 ]

        -- MaybeX.unwrap
        --     []
        --     (\p ->
        --         [ p.distance |> (\d -> roundN 2 (d / 1000)) |> String.fromFloat |> text
        --         , p.speed |> MaybeX.unwrap none ((\s -> roundN 2 (s * 3.6)) >> String.fromFloat >> text)
        --         , p.target |> MaybeX.unwrap none text
        --         ]
        --     )
        --     model.currentPoint
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
            , text <| formatTime <| millisToPosix model.currentTime
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
        , lazy leaderboard model.states
        ]
