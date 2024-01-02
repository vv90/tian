module Components.PlaybackState exposing (PlaybackState, advancePlaybackState, initPlaybackState, setSpeed, start, stop)

import Api.Types exposing (..)
import List.Extra as ListX
import Time exposing (Posix, millisToPosix, posixToMillis)


type alias PlaybackState =
    { speed : Int
    , playing : Bool
    , startTime : Posix
    , endTime : Posix
    , currentTime : Posix
    , trackPoints : List ProgressPoint
    }


stop : PlaybackState -> PlaybackState
stop state =
    { state | playing = False }


start : PlaybackState -> PlaybackState
start state =
    { state | playing = True }


setSpeed : Int -> PlaybackState -> PlaybackState
setSpeed speed state =
    { state | speed = speed }


initPlaybackState : TaskProgress -> Maybe PlaybackState
initPlaybackState { points } =
    let
        firstPoint =
            List.head points

        lastPoint =
            ListX.last points
    in
    Maybe.map2
        (\first last ->
            { speed = 1
            , playing = False
            , startTime = millisToPosix first.time
            , endTime = millisToPosix last.time
            , currentTime = millisToPosix first.time
            , trackPoints = points
            }
        )
        firstPoint
        lastPoint


advancePlaybackState : PlaybackState -> ( Maybe ProgressPoint, PlaybackState )
advancePlaybackState state =
    let
        newTimeMillis =
            posixToMillis state.currentTime + 100 * state.speed

        shouldDropNext : List ProgressPoint -> Bool
        shouldDropNext ps =
            List.head ps
                |> Maybe.map (\p -> p.time < newTimeMillis)
                |> Maybe.withDefault False

        dropPoints : List ProgressPoint -> Maybe ( ProgressPoint, List ProgressPoint )
        dropPoints points =
            ListX.uncons points
                |> Maybe.andThen
                    (\( p, ps ) ->
                        if shouldDropNext ps then
                            dropPoints ps

                        else
                            Just ( p, ps )
                    )
    in
    if shouldDropNext state.trackPoints then
        dropPoints state.trackPoints
            |> Maybe.map
                (\( p, ps ) ->
                    ( Just p, { state | trackPoints = ps, currentTime = millisToPosix newTimeMillis } )
                )
            |> Maybe.withDefault
                ( Nothing, { state | trackPoints = [], currentTime = millisToPosix newTimeMillis, playing = False } )

    else
        ( Nothing, { state | currentTime = millisToPosix newTimeMillis } )
