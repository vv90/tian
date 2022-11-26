module Common.ApiResult exposing (..)

import Common.Deferred exposing (Deferred)
import Http exposing (Error)


type alias ApiResult a =
    Result Error a


type alias DeferredResult a =
    Deferred (ApiResult a)
