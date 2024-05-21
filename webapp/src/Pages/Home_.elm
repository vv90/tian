module Pages.Home_ exposing (page, Model, Msg)

import Element exposing (..)
import View exposing (View)
import Route exposing (Route)
import Page exposing (Page)
import Layouts
import Shared
import Effect

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
    , element =  text "Hello, world!" 
    }
