module Pages.NotFound_ exposing (Model, Msg, page)

import Effect
import Element exposing (..)
import Page exposing (Page)
import Route exposing (Route)
import Shared
import View


type alias Model =
    ()


type alias Msg =
    ()


page : Shared.Model -> Route () -> Page Model Msg
page _ _ =
    Page.new
        { init = \_ -> ( (), Effect.replaceRoute Shared.defaultPage )
        , update = \_ model -> ( model, Effect.none )
        , subscriptions = always Sub.none
        , view = \_ -> View.none
        }
