module LogViewDemo exposing (..)

import Html exposing (..)
import Nav.TaskProgress exposing (TaskProgress, TaskStatus(..), advanceTaskProgress, showTaskStatus)
import Nav.FlightTrack exposing (FlightTrack, TrackPoint, trackPointToGeoPoint) 
import Nav.FlightTask exposing (FlightTask)
import PlaybackState exposing (PlaybackState, initPlaybackState, start, stop, setSpeed, advancePlaybackState)
import List.Nonempty as Nonempty
import Maybe.Extra as MaybeX
import Time exposing (Posix)
import Json.Decode as D
import Html.Events exposing (onClick)
import Html.Events.Extra exposing (onChange)
import Html.Attributes exposing (value)
import TimeUtils exposing (addTime, formatTime)


type alias Model =
  { flightTrack : FlightTrack
  , playbackState : PlaybackState
  , progress : TaskProgress 
  , currentPosition : TrackPoint
  }

init : FlightTask -> FlightTrack -> Model
init flightTask flightTrack =
  { flightTrack = flightTrack
  , playbackState = initPlaybackState flightTrack
  , progress = 
    { task = flightTask
    , status = NotStarted 
    , lastPoint = (Nonempty.head >> trackPointToGeoPoint) flightTrack.points
    }
  , currentPosition = Nonempty.head flightTrack.points
  }
-- init : () -> Model
-- init () =
--   Err NoFile

type Msg
  = PointReached TrackPoint
  | PlaybackStarted
  | PlaybackStopped
  | PlaybackSpeedChanged Int
  | PlaybackTick Posix


update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    -- PointReached trackPoint -> 
    --   ({ progress = advance trackPoint model.progress }, Cmd.none)
    
    PlaybackStarted -> 
      ( { model 
        | playbackState = start model.playbackState 
        }
      , Cmd.none
      )

    PlaybackStopped ->
      ( { model 
        | playbackState = stop model.playbackState
        }
      , Cmd.none
      )

    PlaybackSpeedChanged speed ->
      ( { model 
        | playbackState = setSpeed speed model.playbackState
        }
      , Cmd.none
      ) 
    
    PlaybackTick _ ->
      let
        (nextPoint, nextPlaybackState) = advancePlaybackState model.playbackState
      in
      
      ( { model
        | playbackState = nextPlaybackState
        , progress = 
            MaybeX.unwrap 
              model.progress
              (\p -> advanceTaskProgress p model.progress)
              nextPoint
        , currentPosition = 
            MaybeX.unwrap
              model.currentPosition
              identity
              nextPoint
        }
      , Cmd.none
      )

    _ -> (model, Cmd.none)

subscriptions : Model -> Sub Msg
subscriptions model =
  if 
    model.playbackState.playing
  then 
    Time.every 100 PlaybackTick
  else 
    Sub.none

view : Model -> Html Msg
view model = 
  div 
    []
    [ viewReplayControls model.playbackState
    , viewTaskProgress model.progress
    ]

viewReplayControls : PlaybackState -> Html Msg
viewReplayControls state =
  div 
    [ ]
    [ select [ onChange (D.decodeString D.int >> Result.withDefault 1 >> PlaybackSpeedChanged) ] 
      [ option [ value "1" ] [ text "1x" ] 
      , option [ value "10" ] [ text "10x"]
      , option [ value "100" ] [ text "100x"]
      ] 
    , viewPlayButton state
    , viewPlaybackState state
    ]

viewPlayButton : PlaybackState -> Html Msg
viewPlayButton state =
  if 
    state.playing 
  then 
    button [onClick PlaybackStopped] [ text "Pause" ]
  else
    button [onClick PlaybackStarted] [ text "Play" ]

viewPlaybackState : PlaybackState -> Html Msg
viewPlaybackState state = 
  div 
    []
    [ addTime state.startTime state.currentTime |> formatTime |> text
    , state.speed |> String.fromInt |> text |> List.singleton |> p [] 
    ]

viewTaskProgress : TaskProgress -> Html Msg
viewTaskProgress progress = 
  p 
    [] 
    [ showTaskStatus progress.status |> text ]

-- view : (Result TrackLoadError FlightTrack) -> Model -> Html msg
-- view (rTrack) model =
--   case rTrack of
--     Ok track -> 
--       div [] 
--         [ (showFlightTrack >> text >> List.singleton >> p[]) track 
--         , (showTaskStatus >> text >> List.singleton >> p[]) model.progress.status
--         ]
--     Err NoFile -> 
--       text "No file"
--     Err (TrackParsingError es) -> 
--       showParseErrors es |> text