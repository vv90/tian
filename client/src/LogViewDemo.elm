module LogViewDemo exposing (..)

import Html exposing (..)
import Nav.FlightTrack exposing (TrackLoadError(..), FlightTrack)
import Date exposing (formatDate)
import Utils exposing (showParseErrors)
import Nav.FlightTrack exposing (showFlightTrack)

view : (Result TrackLoadError FlightTrack) -> Html msg
view (rTrack) =
  case rTrack of
    Ok track -> 
      div [] 
        [ showFlightTrack track |> text ]
    Err NoFile -> 
      text "No file"
    Err (TrackParsingError es) -> 
      showParseErrors es |> text