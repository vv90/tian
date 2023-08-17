module Api.NavPoint exposing
    ( NavPoint
    , WaypointStyle(..)
    , navPointDecoder
    , navPointEncoder
    , waypointStyleDecoder
    , waypointStyleEncoder
    )

import Api.Geo
import Json.Decode
import Json.Decode.Pipeline
import Json.Encode
import Maybe.Extra


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


waypointStyleEncoder : WaypointStyle -> Json.Encode.Value
waypointStyleEncoder a =
    case a of
        Unknown ->
            Json.Encode.string "Unknown"

        Waypoint ->
            Json.Encode.string "Waypoint"

        AirfieldGrass ->
            Json.Encode.string "AirfieldGrass"

        Outlanding ->
            Json.Encode.string "Outlanding"

        AirfieldGliding ->
            Json.Encode.string "AirfieldGliding"

        AirfieldSolid ->
            Json.Encode.string "AirfieldSolid"

        MountainPass ->
            Json.Encode.string "MountainPass"

        MountainTop ->
            Json.Encode.string "MountainTop"

        TransmitterMast ->
            Json.Encode.string "TransmitterMast"

        VOR ->
            Json.Encode.string "VOR"

        NDB ->
            Json.Encode.string "NDB"

        CoolingTower ->
            Json.Encode.string "CoolingTower"

        Dam ->
            Json.Encode.string "Dam"

        Tunnel ->
            Json.Encode.string "Tunnel"

        Bridge ->
            Json.Encode.string "Bridge"

        PowerPlant ->
            Json.Encode.string "PowerPlant"

        Castle ->
            Json.Encode.string "Castle"

        Intersection ->
            Json.Encode.string "Intersection"


waypointStyleDecoder : Json.Decode.Decoder WaypointStyle
waypointStyleDecoder =
    Json.Decode.string
        |> Json.Decode.andThen
            (\a ->
                case a of
                    "Unknown" ->
                        Json.Decode.succeed Unknown

                    "Waypoint" ->
                        Json.Decode.succeed Waypoint

                    "AirfieldGrass" ->
                        Json.Decode.succeed AirfieldGrass

                    "Outlanding" ->
                        Json.Decode.succeed Outlanding

                    "AirfieldGliding" ->
                        Json.Decode.succeed AirfieldGliding

                    "AirfieldSolid" ->
                        Json.Decode.succeed AirfieldSolid

                    "MountainPass" ->
                        Json.Decode.succeed MountainPass

                    "MountainTop" ->
                        Json.Decode.succeed MountainTop

                    "TransmitterMast" ->
                        Json.Decode.succeed TransmitterMast

                    "VOR" ->
                        Json.Decode.succeed VOR

                    "NDB" ->
                        Json.Decode.succeed NDB

                    "CoolingTower" ->
                        Json.Decode.succeed CoolingTower

                    "Dam" ->
                        Json.Decode.succeed Dam

                    "Tunnel" ->
                        Json.Decode.succeed Tunnel

                    "Bridge" ->
                        Json.Decode.succeed Bridge

                    "PowerPlant" ->
                        Json.Decode.succeed PowerPlant

                    "Castle" ->
                        Json.Decode.succeed Castle

                    "Intersection" ->
                        Json.Decode.succeed Intersection

                    _ ->
                        Json.Decode.fail "No matching constructor"
            )


type alias NavPoint =
    { name : String
    , code : String
    , country : Maybe String
    , lat : Api.Geo.Latitude
    , lon : Api.Geo.Longitude
    , elev : Api.Geo.Elevation
    , style : WaypointStyle
    , rwdir : Maybe Api.Geo.Direction
    , rwlen : Maybe Api.Geo.Distance
    , freq : Maybe String
    , desc : String
    }


navPointEncoder : NavPoint -> Json.Encode.Value
navPointEncoder a =
    Json.Encode.object
        [ ( "name", Json.Encode.string a.name )
        , ( "code", Json.Encode.string a.code )
        , ( "country", Maybe.Extra.unwrap Json.Encode.null Json.Encode.string a.country )
        , ( "lat", Api.Geo.latitudeEncoder a.lat )
        , ( "lon", Api.Geo.longitudeEncoder a.lon )
        , ( "elev", Api.Geo.elevationEncoder a.elev )
        , ( "style", waypointStyleEncoder a.style )
        , ( "rwdir", Maybe.Extra.unwrap Json.Encode.null Api.Geo.directionEncoder a.rwdir )
        , ( "rwlen", Maybe.Extra.unwrap Json.Encode.null Api.Geo.distanceEncoder a.rwlen )
        , ( "freq", Maybe.Extra.unwrap Json.Encode.null Json.Encode.string a.freq )
        , ( "desc", Json.Encode.string a.desc )
        ]


navPointDecoder : Json.Decode.Decoder NavPoint
navPointDecoder =
    Json.Decode.succeed NavPoint
        |> Json.Decode.Pipeline.required "name" Json.Decode.string
        |> Json.Decode.Pipeline.required "code" Json.Decode.string
        |> Json.Decode.Pipeline.required "country" (Json.Decode.nullable Json.Decode.string)
        |> Json.Decode.Pipeline.required "lat" Api.Geo.latitudeDecoder
        |> Json.Decode.Pipeline.required "lon" Api.Geo.longitudeDecoder
        |> Json.Decode.Pipeline.required "elev" Api.Geo.elevationDecoder
        |> Json.Decode.Pipeline.required "style" waypointStyleDecoder
        |> Json.Decode.Pipeline.required "rwdir" (Json.Decode.nullable Api.Geo.directionDecoder)
        |> Json.Decode.Pipeline.required "rwlen" (Json.Decode.nullable Api.Geo.distanceDecoder)
        |> Json.Decode.Pipeline.required "freq" (Json.Decode.nullable Json.Decode.string)
        |> Json.Decode.Pipeline.required "desc" Json.Decode.string
