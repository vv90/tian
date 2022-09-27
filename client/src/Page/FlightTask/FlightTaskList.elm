module Page.FlightTask.FlightTaskList exposing (..)

import Api.Entity exposing (Entity)
import Api.FlightTask exposing (FlightTask)
import Element exposing (Element, fill, none, table)


e : Entity Int FlightTask -> ()
e _ =
    ()


type alias Model =
    List (Entity Int FlightTask)


view : List (Entity Int FlightTask) -> Element msg
view flightTasks =
    let
        descriptionView : Entity Int FlightTask -> Element msg
        -- descriptionView (Entity key (FlightTask start turnpoints finish)) =
        descriptionView x =
            none
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
