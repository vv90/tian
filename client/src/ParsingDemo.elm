module ParsingDemo exposing (..)

import Geo.GeoUtils exposing (Latitude(..))
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Maybe.Extra as MaybeX
import Nav.FlightTrack exposing (FlightInfo, FlightTrack, FlightTrackAggregateValue, flightInfoParser, flightTrackParser, showFlightInfo, showFlightTrack)
import Nav.NavPoint exposing (..)
import Nav.Units exposing (Deg(..))
import Parser exposing (..)
import Utils exposing (showParseResult)
import Utils.Date exposing (formatDate)
import Utils.ParserUtils exposing (..)



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


type Msg
    = InputChanged String


update : Msg -> Model -> ( Model, Cmd Msg )
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
        , h2 [] [ showParseResult model.output (\_ -> "OK") |> text ]
        ]
