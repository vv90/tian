module Page.FlightTask.FlightTaskPreview exposing (..)

import Api.Types exposing (..)
import Element exposing (Element, text)


view : FlightTask -> Element msg
view flightTask =
    text <|
        String.join
            " - "
            ((Tuple.first >> .name) flightTask.start
                :: List.map (Tuple.first >> .name) flightTask.turnpoints
                ++ [ (Tuple.first >> .name) flightTask.finish ]
            )
