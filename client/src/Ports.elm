port module Ports exposing (..)


port startDemo : () -> Cmd msg


port messageReceiver : (String -> msg) -> Sub msg
