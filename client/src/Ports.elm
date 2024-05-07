port module Ports exposing (flightPositionReceiver, messageReceiver, onboardingCompleted, startDemo, watchFlight)


port startDemo : () -> Cmd msg


port messageReceiver : (String -> msg) -> Sub msg


port watchFlight : () -> Cmd msg


port flightPositionReceiver : (String -> msg) -> Sub msg


port onboardingCompleted : () -> Cmd msg
