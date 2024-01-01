module Page.FlightTask.FlightTaskList exposing (Props, view, viewLoaded)

import Api.Types exposing (..)
import Common.ApiResult exposing (ApiResult)
import Common.Deferred exposing (Deferred(..))
import Common.Palette as Palette
import Element exposing (Element, column, mouseOver, none, shrink, spacing, table, text)
import Element.Background as Background
import Element.Input as Input
import Page.FlightTask.FlightTaskPreview as FlightTaskPreview



-- type alias Model =
--     { selectedFlightTaskId : Maybe Int
--     }
-- init : Model
-- init =
--     { selectedFlightTaskId = Nothing
--     }
-- type Msg
--     = FlightTaskSelected Int
-- type Effect
--     = DummyEffect
-- update : Msg -> Model -> ( Model, Cmd Msg, Effect )
-- update msg model =
--     case msg of
--         FlightTaskSelected flightTaskId ->
--             ( { model | selectedFlightTaskId = Just flightTaskId }
--             , Cmd.none
--             , DummyEffect
--             )


viewLoaded : List (Entity Int FlightTask) -> (Int -> msg) -> Element msg
viewLoaded flightTasks onTaskSelected =
    let
        descriptionView : Entity Int FlightTask -> Element msg
        descriptionView { key, entity } =
            Input.button
                -- [ selectedBackground key
                [ mouseOver [ Background.color Palette.lightGray ]
                ]
                { onPress = Just <| onTaskSelected key
                , label = FlightTaskPreview.view entity
                }

        -- actionsView : Entity Int FlightTask -> Element Msg
        -- actionsView { key } =
        --     Input.button
        --         [ mouseOver [ Background.color Palette.lightGray ] ]
        --         { onPress = Nothing
        --         , label = text "Upload"
        --         }
    in
    table [ spacing 10 ]
        { data = flightTasks
        , columns =
            [ { header = none
              , width = shrink
              , view = descriptionView
              }

            -- , { header = none
            --   , width = fill
            --   , view = actionsView
            --   }
            ]
        }


type alias Props msg =
    { flightTasks : Deferred (ApiResult (List (Entity Int FlightTask)))
    , onTaskSelected : Int -> msg
    , onCreateTaskTriggered : msg
    }


view : Props msg -> Element msg
view { flightTasks, onTaskSelected, onCreateTaskTriggered } =
    let
        backBtn =
            Input.button
                [ mouseOver [ Background.color Palette.lightGray ] ]
                { onPress = Just onCreateTaskTriggered
                , label = text "Create Task"
                }
    in
    case flightTasks of
        Resolved (Ok []) ->
            column [ spacing 10 ]
                [ text "No flight tasks"
                , backBtn
                ]

        Resolved (Ok fts) ->
            column [ spacing 10 ]
                [ viewLoaded fts onTaskSelected
                , backBtn
                ]

        Resolved (Err _) ->
            text "Failed to load flight tasks"

        _ ->
            text "Loading ..."
