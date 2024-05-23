module Pages.Home_ exposing (Model, Msg, page)

import Effect
import Element exposing (..)
import Layouts
import Page exposing (Page)
import Ports
import Route exposing (Route)
import Shared
import View exposing (View)



-- temporary


type alias Model =
    ()


type alias Msg =
    ()


page : Shared.Model -> Route () -> Page Model Msg
page _ _ =
    Page.new
        { init = always ( (), Effect.none )
        , update = \_ _ -> ( (), Effect.none )
        , subscriptions = always Sub.none
        , view = view
        }
        |> Page.withLayout (always <| Layouts.WebappLayout {})


view : Model -> View Msg
view model =
    { title = "Homepage"
    , attributes = []
    , element = text "Hello, world!"
    }
