module Owl.Globe exposing (..)

import Angle exposing (Angle)
import Api.Geo exposing (Distance)
import Axis3d exposing (Axis3d)
import Camera3d exposing (Camera3d)
import Common.GeoUtils exposing (GeoPoint)
import Flags exposing (WindowSize)
import Length exposing (Meters)
import MapUtils exposing (ZoomLevel)
import Pixels exposing (Pixels)
import Plane3d exposing (Plane3d)
import Point2d exposing (Point2d)
import Point3d exposing (Point3d)
import Polygon2d exposing (Polygon2d)
import Quantity exposing (Quantity)
import Rectangle2d exposing (Rectangle2d)
import Scene3d.Mesh as Mesh exposing (Mesh)
import Viewpoint3d


type WorldCoords
    = WorldCoords


type WGS84Coords
    = WGS84Coords


type WGS84Degrees
    = WGS84Degrees


type alias ViewArgs =
    { focalPoint : Point3d Meters WorldCoords -- the center point we're looking at
    , azimuth : Angle -- camera angle around the z axis
    , elevation : Angle -- camera angle relative to xy plane
    , distance : Quantity Float Meters -- camera distance from the focal point
    }


camera : ViewArgs -> Camera3d Meters WorldCoords
camera viewArgs =
    Camera3d.orthographic
        { viewpoint =
            Viewpoint3d.orbitZ viewArgs
        , viewportHeight = viewArgs.distance

        -- , verticalFieldOfView = Angle.degrees 30
        }


screenRectangle : WindowSize -> Rectangle2d Pixels screenCoords
screenRectangle windowSize =
    Rectangle2d.from
        Point2d.origin
        (Point2d.pixels
            (toFloat windowSize.width)
            (toFloat windowSize.height)
        )



-- fieldOfViewPolygon : WindowSize -> ViewArgs -> Polygon2d units coords
-- fieldOfViewPolygon windowSize viewArgs =
--     let
--         cmr = camera viewArgs
--         sRect = screenRectangle windowSize
--         rolyPoly =
--             Rectangle2d.vertices sRect
--             |> List.map (Camera3d.ray cmr sRect >> Axis3d.intersectionWithPlane Plane3d.xy)
--     in
--     ()
