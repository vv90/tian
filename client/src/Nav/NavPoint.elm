module Nav.NavPoint exposing (..)

import Geo.GeoUtils exposing (GeoPoint, Latitude(..), Longitude(..), toDecimalDegrees)
import Nav.Units exposing (Deg(..))
import Parser exposing (..)
import ParserUtils exposing (..)
import Set


type WaypointStyle
    = Unknown
    | Waypoint
    | AirfieldGrass
    | Outlanding
    | AirfieldGliding
    | AirfieldSolid
    | MountainPass
    | MountainTop
    | TransmitterMast
    | VOR
    | NDB
    | CoolingTower
    | Dam
    | Tunnel
    | Bridge
    | PowerPlant
    | Castle
    | Intersection


waypointStyleParser : Parser WaypointStyle
waypointStyleParser =
    oneOf
        [ succeed Unknown |. symbol "0"
        , succeed Waypoint |. symbol "1"
        , succeed AirfieldGrass |. symbol "2"
        , succeed Outlanding |. symbol "3"
        , succeed AirfieldGliding |. symbol "4"
        , succeed AirfieldSolid |. symbol "5"
        , succeed MountainPass |. symbol "6"
        , succeed MountainTop |. symbol "7"
        , succeed TransmitterMast |. symbol "8"
        , succeed VOR |. symbol "9"
        , succeed NDB |. symbol "10"
        , succeed CoolingTower |. symbol "11"
        , succeed Dam |. symbol "12"
        , succeed Tunnel |. symbol "13"
        , succeed Bridge |. symbol "14"
        , succeed PowerPlant |. symbol "15"
        , succeed Castle |. symbol "16"
        , succeed Intersection |. symbol "17"
        , succeed Unknown
        ]



-- type Lat = North Int Float | South Int Float
-- type Lon = East Int Float | West Int Float


type Elev
    = ElevMeters Float
    | ElevFeet Float


type RwLen
    = LenMeters Float
    | LenNauticalMiles Float
    | LenStatuteMiles Float


type alias NavPoint =
    { name : String
    , code : String
    , country : Maybe String
    , geoPoint : GeoPoint
    , elev : Elev
    , style : WaypointStyle
    , rwdir : Maybe Int
    , rwlen : Maybe RwLen
    , freq : Maybe ( Int, Int )
    , desc : String
    }


nameParser : Parser String
nameParser =
    let
        validChar : Char -> Bool
        validChar c =
            Char.isAlphaNum c || (Set.fromList [ '\'', '_', '-' ] |> Set.member c)
    in
    variable
        { start = validChar
        , inner = validChar
        , reserved = Set.empty
        }


geoPointParser : Parser GeoPoint
geoPointParser =
    succeed (\lat lon -> GeoPoint lon lat)
        |= latParser
        |. symbol ","
        |= lonParser


elevParser : Parser Elev
elevParser =
    succeed (\x f -> f x)
        |= float
        |= oneOf
            [ succeed ElevMeters |. symbol "m"
            , succeed ElevFeet |. symbol "ft"
            ]


rwLenParser : Parser RwLen
rwLenParser =
    succeed (\x f -> f x)
        |= float
        |= oneOf
            [ succeed LenMeters |. symbol "m"
            , succeed LenNauticalMiles |. symbol "nm"
            , succeed LenStatuteMiles |. symbol "ml"
            ]


freqParser : Parser ( Int, Int )
freqParser =
    succeed Tuple.pair
        |= plainIntParser
        |. symbol "."
        |= plainIntParser


descParser : Parser String
descParser =
    succeed identity
        |. symbol "\""
        |= getChompedString (succeed () |. chompWhile (\c -> c /= '"'))


navPointParser : Parser NavPoint
navPointParser =
    succeed NavPoint
        |= inQuotations nameParser
        |. symbol ","
        |= nameParser
        |. symbol ","
        |= maybeParser wordVal
        |. symbol ","
        |= geoPointParser
        |. symbol ","
        |= elevParser
        |. symbol ","
        |= waypointStyleParser
        |. symbol ","
        |= maybeParser int
        |. symbol ","
        |= maybeParser rwLenParser
        |. symbol ","
        |= (freqParser |> inQuotations |> maybeParser)
        |. symbol ","
        |= descParser
