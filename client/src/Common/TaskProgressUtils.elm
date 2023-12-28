module Common.TaskProgressUtils exposing (..)

import Api.Geo exposing (Latitude(..))
import Api.NavPoint exposing (NavPoint)
import Api.TaskProgress exposing (ProgressPoint)
import MapUtils exposing (LineStyle(..), MapItem(..), PointStyle(..))


progressPointsToMapItems : List ProgressPoint -> List MapItem
progressPointsToMapItems progressPoints =
    let
        pointMapItem : ProgressPoint -> MapItem
        pointMapItem p =
            Point TrackPoint { lat = p.lat, lon = p.lon }

        lineMapItem : Maybe ProgressPoint -> ProgressPoint -> Maybe MapItem
        lineMapItem lastPoint p =
            Maybe.map
                (\lp -> Line TrackLine [ { lat = lp.lat, lon = lp.lon }, { lat = p.lat, lon = p.lon } ])
                lastPoint

        combineMapItems : List MapItem -> MapItem -> Maybe MapItem -> List MapItem
        combineMapItems prev pmi lmi =
            case lmi of
                Just x ->
                    x :: prev

                Nothing ->
                    prev

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


targetToMapItem : ProgressPoint -> NavPoint -> MapItem
targetToMapItem pp np =
    Line TaskLine [ { lat = pp.lat, lon = pp.lon }, { lat = np.lat, lon = np.lon } ]
