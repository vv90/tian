module ParsingDemo exposing (..)

import Nav.NavPoint exposing (..)

import Parser exposing (..)
import ParserUtils exposing (..)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Utils exposing (showParseResult)
import Geo.GeoUtils exposing (Latitude(..))
import Nav.Units exposing (Deg(..))
import Nav.FlightTrack exposing (FlightTrack, flightTrackParser, showFlightTrack, showFlightInfo, FlightInfo, flightInfoParser)
import Date exposing (formatDate)
import Maybe.Extra as MaybeX
import Nav.FlightTrack exposing (FlightTrackAggregateValue)


-- demoParser : Parser String
-- demoParser = succeed ""

type alias Model = 
  { input : String
  , output : Result (List DeadEnd) FlightTrackAggregateValue
  }

init : () -> Model
init () =
  { input = ""
  , output = Err []
  }

type Msg =
  InputChanged String

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    InputChanged s -> 
      ( { model | output = Parser.run flightTrackParser s }
      , Cmd.none
      )

view : Model -> Html Msg
view model =
  div 
    []
    [ textarea [ onInput InputChanged ] []
    , h2 [] [showParseResult model.output (\_ -> "OK") |> text] ]