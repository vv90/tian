module Flags exposing (Config, Flags, WindowSize)


type alias Flags =
    { windowSize : WindowSize
    , config : Config
    }


type alias WindowSize =
    { height : Int
    , width : Int
    }


type alias Config =
    { backendUrl : String }
