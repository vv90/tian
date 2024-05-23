port module Ports exposing (flightPositionReceiver, onboardingCompleted, watchFlight)


port watchFlight : () -> Cmd msg


port flightPositionReceiver : (String -> msg) -> Sub msg


port onboardingCompleted : () -> Cmd msg
