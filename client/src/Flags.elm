module Flags exposing (..)

type alias Flags =
  { backendUrl : String
  , windowSize : WindowSize
  }


type alias WindowSize =
  { height : Int
  , width : Int
  }
