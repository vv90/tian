module PointDemo exposing (..)
import Html exposing (..)
import Geo.GeoUtils exposing (GeoPoint)
import Nav.Units exposing (Meters(..), getMeters, Deg(..), getDeg)
import Geo.GeoUtils exposing (getLon, getLat, bearing)
import Html.Attributes as A exposing (type_, min, max)
import Html.Events exposing (onInput)
import Parser as Parser exposing (int, float)
import Geo.GeoUtils exposing (destination)

type alias Model =
  { heading: Deg
  , distance: Meters
  , projectedPoint: Maybe GeoPoint
  }

init : () -> Model 
init _ =
  { heading = Deg 0 
  , distance = Meters 1000
  , projectedPoint = Nothing
  }

type OutMsg 
  = Pass
  | PointGenerated GeoPoint

type Msg 
  = None
  | DistanceChanged (Maybe GeoPoint) String
  | HeadingChanged (Maybe GeoPoint) String


update : Msg -> Model -> (Model, Cmd Msg)
update msg model = 
  case msg of 
    None -> (model, Cmd.none)
    DistanceChanged origin d -> 
      let
        newDistance = d |> Parser.run Parser.float |> Result.map Meters |> Result.withDefault model.distance
      in
        ( { model 
          | distance = newDistance
          , projectedPoint = origin |> Maybe.map (destination model.heading newDistance)}
        , Cmd.none
        )
    HeadingChanged origin h -> 
      let
        newHeading = h |> Parser.run Parser.float |> Result.map Deg |> Result.withDefault model.heading 
      in
      
      ( { model 
        | heading = newHeading
        , projectedPoint = origin |> Maybe.map (destination newHeading model.distance)
        }
      , Cmd.none
      )

view : GeoPoint -> Maybe GeoPoint -> Html Msg
view cursorPoint selectedPoint = 
  let
    headingToCursor = Maybe.map (\x -> bearing x cursorPoint) selectedPoint
  in
    div 
      []
      [ input [ A.type_ "number", A.step "1", onInput (DistanceChanged selectedPoint)] []
      , input [ A.type_ "range", A.min "0", A.max "360", onInput (HeadingChanged selectedPoint) ] [] 
      , h2 [] [ selectedPoint |> Maybe.map showPoint |> Maybe.withDefault "--, --" |> text ]
      , h2 [] [ headingToCursor |> Maybe.map (getDeg >> String.fromFloat) |> Maybe.withDefault "" |> text ] ]

showPoint : GeoPoint -> String
showPoint geoPoint =
  let
    lonDeg = geoPoint.lon |> getLon |> getDeg
    latDeg = geoPoint.lat |> getLat |> getDeg
  in
  
    String.fromFloat lonDeg ++ ", " ++ String.fromFloat latDeg