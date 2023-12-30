port module Ports exposing (..)


port startDemo : () -> Cmd msg


port messageReceiver : (String -> msg) -> Sub msg


port watchFlight : () -> Cmd msg


port flightPositionReceiver : (String -> msg) -> Sub msg
