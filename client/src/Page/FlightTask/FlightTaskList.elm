module Page.FlightTask.FlightTaskList exposing (..)

import Api.Entity exposing (Entity)
import Api.FlightTask exposing (FlightTask)
import Element exposing (Element, el, fill, mouseOver, none, table, text)
import Element.Background as Background
import Element.Input as Input
import List.Extra
import Utils.ApiResult exposing (ApiResult)
import Utils.Deferred exposing (Deferred(..))
import Utils.Palette as Palette


type alias Model =
    { selectedFlightTaskId : Maybe Int
    }


init : Model
init =
    { selectedFlightTaskId = Nothing
    }


type Msg
    = FlightTaskSelected Int


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        FlightTaskSelected flightTaskId ->
            ( { model | selectedFlightTaskId = Just flightTaskId }
            , Cmd.none
            )


selectedFlightTask : Deferred (ApiResult (List (Entity Int FlightTask))) -> Model -> Maybe (Entity Int FlightTask)
selectedFlightTask flightTasksD model =
    case ( flightTasksD, model.selectedFlightTaskId ) of
        ( Resolved (Ok items), Just id ) ->
            List.Extra.find (\{ key } -> key == id) items

        _ ->
            Nothing


viewLoaded : List (Entity Int FlightTask) -> Model -> Element Msg
viewLoaded flightTasks model =
    let
        selectedBackground key =
            if model.selectedFlightTaskId == Just key then
                Background.color Palette.primaryLight

            else
                Background.color Palette.tarnsparent

        labelText : FlightTask -> String
        labelText ft =
            String.join
                " - "
                ((Tuple.first >> .name) ft.start
                    :: List.map (Tuple.first >> .name) ft.turnpoints
                    ++ [ (Tuple.first >> .name) ft.finish ]
                )

        descriptionView : Entity Int FlightTask -> Element Msg
        descriptionView { key, entity } =
            Input.button
                [ selectedBackground key
                , mouseOver [ Background.color Palette.lightGray ]
                ]
                { onPress = Just <| FlightTaskSelected key
                , label = text <| labelText entity
                }
    in
    table []
        { data = flightTasks
        , columns =
            [ { header = none
              , width = fill
              , view = descriptionView
              }
            ]
        }


view : Deferred (ApiResult (List (Entity Int FlightTask))) -> Model -> Element Msg
view flightTasksD model =
    case flightTasksD of
        Resolved (Ok flightTasks) ->
            viewLoaded flightTasks model

        Resolved (Err _) ->
            text "Failed to load flight tasks"

        _ ->
            text "Loading ..."
