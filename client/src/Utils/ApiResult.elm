module Utils.ApiResult exposing (..)

import Http exposing (Error)


type alias ApiResult a =
    Result Error a
