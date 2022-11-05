module Common.TaskProgressUtils exposing (..)

import Api.Geo exposing (Latitude(..))
import Api.TaskProgress exposing (ProgressPoint)
import MapUtils exposing (LineStyle(..), MapItem(..), PointStyle(..))


progressPointsToMapItems : List ProgressPoint -> List MapItem
progressPointsToMapItems progressPoints =
    let
        pointMapItem : ProgressPoint -> MapItem
        pointMapItem p =
            Point TrackPoint ( p.lat, p.lon )

        lineMapItem : Maybe ProgressPoint -> ProgressPoint -> Maybe MapItem
        lineMapItem lastPoint p =
            Maybe.map
                (\lp -> Line TrackLine [ ( lp.lat, lp.lon ), ( p.lat, p.lon ) ])
                lastPoint

        combineMapItems : List MapItem -> MapItem -> Maybe MapItem -> List MapItem
        combineMapItems prev pmi lmi =
            case lmi of
                Just x ->
                    pmi :: x :: prev

                Nothing ->
                    pmi :: prev

        buildMapItemsStep : List MapItem -> Maybe ProgressPoint -> List ProgressPoint -> List MapItem
        buildMapItemsStep prev lastPoint points =
            case points of
                [] ->
                    prev

                p :: ps ->
                    buildMapItemsStep
                        (combineMapItems prev (pointMapItem p) (lineMapItem lastPoint p))
                        (Just p)
                        ps
    in
    buildMapItemsStep [] Nothing progressPoints
        |> List.reverse
