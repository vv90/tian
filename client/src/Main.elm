module Main exposing (..)

import Browser
import Html exposing (Html, button, div, text)
import Html.Events exposing (onClick)
import Map as Map exposing (..)
import MapUtils exposing (..)
import Geo.GeoUtils exposing (..)
import Flags exposing (..)
import Nav.NavPoint exposing (navPointParser, NavPoint)
import Nav.NavPointList exposing (navPointLines)
import Parser as Parser
import Utils exposing (..)
import List.Extra
import Result.Extra
import Nav.Units exposing (..)
import Nav.FlightTask exposing (Turnpoint(..), FlightTask, TaskStart(..), TaskFinish(..), taskToMapItems)

main : Program Flags Model Msg
main =
  Browser.element 
    { init = init
    , update = update
    , view = view 
    , subscriptions = subscriptions
    }

type alias Model = 
  { mapModel : Map.Model
  }



lookupNavPoint : String -> List NavPoint -> Result MapInitError NavPoint
lookupNavPoint name navPoints =
  List.Extra.find (\np -> np.name == name) navPoints
  |> Result.fromMaybe ("Point " ++ name ++ " not found")
  |> Result.mapError LookupError

-- turnPointsToMapItems : List Turnpoint -> List MapItem
-- turnPointsToMapItems turnpoints = 
--   let
--     turnpointToItem tp =
--       case tp of
--         Cylinder r p -> Circle p r
--         Line r p -> Point p

--   in
--     List.map turnpointToItem turnpoints

init : Flags -> (Model, Cmd Msg)
init flags =
  let
    navPointsResult : Result MapInitError (List NavPoint)
    navPointsResult = navPointLines |> List.map (Parser.run navPointParser) |> Result.Extra.combine |> Result.mapError ParsingError

    fromNavPointName : String -> Result MapInitError NavPoint
    fromNavPointName = \pointName -> navPointsResult |> Result.andThen (lookupNavPoint pointName) 

    initTaskPoint : (String, a) -> Result MapInitError (NavPoint, a)
    initTaskPoint = Tuple.mapFirst fromNavPointName >> Result.Extra.combineFirst
    
    start = initTaskPoint ("USMAER", StartLine (Meters 5e3))
    finish = initTaskPoint ("USMAER", FinishCylinder (Meters 3e3))
    turnpoints = 
      [ ("U15PANINO", Cylinder (Meters 5e2))
      , ("U43ISRAIL", Cylinder (Meters 5e2))
      , ("U21BITYG", Cylinder (Meters 5e2))
      , ("U44TOKAR", Cylinder (Meters 5e2))
      , ("U14HAVA", Cylinder (Meters 5e2))
      , ("U12DEVI", Cylinder (Meters 5e2))
      ] 
      |> List.map initTaskPoint
      |> Result.Extra.combine


    flightTask = 
      Result.map3 FlightTask start turnpoints finish

    mapItems = flightTask |> Result.map taskToMapItems

    mapModel = 
      Map.init 
        flags.windowSize
        (Result.withDefault [] mapItems)
        9 
        { lon = LonDeg (Deg 39.662962)
        , lat = LatDeg (Deg 52.030558) 
        } 
  in 
    ( { mapModel = mapModel
      }
    , Cmd.none
    )



type Msg = MapMsg Map.Msg | Nothing

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    MapMsg m -> 
      let
        (nextModel, cmd) = Map.update m model.mapModel
      in
        ({ model | mapModel = nextModel}, Cmd.map MapMsg cmd)
    Nothing -> (model, Cmd.none)

subscriptions : Model -> Sub Msg
subscriptions model = 
  Sub.map MapMsg (Map.subscriptions model.mapModel)

view : Model -> Html Msg
view model =
  div []
    [ Map.view model.mapModel |> Html.map MapMsg
    ]