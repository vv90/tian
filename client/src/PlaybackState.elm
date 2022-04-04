module PlaybackState exposing (..)
import Time exposing (Posix, millisToPosix, posixToMillis)
import Nav.FlightTrack exposing (FlightTrack, TrackPoint)
import Array
import Maybe.Extra as MaybeX
import List.Extra as ListX

-- type PlaybackSpeed = X1 | X10 | X100

-- playbackSpeedToInt : PlaybackSpeed -> Int
-- playbackSpeedToInt speed =
--   case speed of
--     X1 -> 1
--     X10 -> 10
--     X100 -> 100

type alias PlaybackState = 
  { speed: Int
  , playing: Bool
  , startTime: Posix
  , endTime: Posix
  , currentTime: Posix
  , trackPoints: List TrackPoint
  }


initPlaybackState : FlightTrack -> PlaybackState 
initPlaybackState track =
  let
    start = 
      Array.get 0 track.points 
      |> MaybeX.unwrap (millisToPosix 0) (\p -> p.time)
    end = 
      Array.get (Array.length track.points - 1) track.points
      |> MaybeX.unwrap (millisToPosix 0) (\p -> p.time)
  in
    { speed = 1 
    , playing = False
    , startTime = start
    , endTime = end
    , currentTime = start
    , trackPoints = Array.toList track.points
    }

advancePlaybackState : PlaybackState -> (Maybe TrackPoint, PlaybackState)
advancePlaybackState state =
  let
    newTimeMillis = posixToMillis state.currentTime + 100 * state.speed

    shouldDropNext : List TrackPoint -> Bool
    shouldDropNext ps =
      List.head ps
      |> Maybe.map (\p -> posixToMillis p.time < newTimeMillis)
      |> Maybe.withDefault False

    dropPoints : List TrackPoint -> Maybe (TrackPoint, List TrackPoint)
    dropPoints points = 
      ListX.uncons points
      |> Maybe.andThen (\(p, ps) -> if shouldDropNext ps then dropPoints ps else Just (p, ps) )

  in
    if 
      shouldDropNext state.trackPoints
    then
      dropPoints state.trackPoints 
        |> Maybe.map 
          (\(p, ps) -> 
            (Just p, { state | trackPoints = ps, currentTime = millisToPosix newTimeMillis }))
        |> Maybe.withDefault 
          (Nothing, { state | trackPoints = [], currentTime = millisToPosix newTimeMillis, playing = False })
    else 
      (Nothing, { state | currentTime = millisToPosix newTimeMillis })
  
  