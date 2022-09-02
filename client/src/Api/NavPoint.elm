module Api.NavPoint exposing
    ( Direction(..)
    , Elevation(..)
    , Latitude(..)
    , Length(..)
    , Longitude(..)
    , NavPoint
    , WaypointStyle(..)
    , directionDecoder
    , directionEncoder
    , elevationDecoder
    , elevationEncoder
    , latitudeDecoder
    , latitudeEncoder
    , lengthDecoder
    , lengthEncoder
    , longitudeDecoder
    , longitudeEncoder
    , navPointDecoder
    , navPointEncoder
    , waypointStyleDecoder
    , waypointStyleEncoder
    )

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


type Latitude
    = LatitudeDegrees Float


latitudeEncoder : Latitude -> Json.Encode.Value
latitudeEncoder a =
    case a of
        LatitudeDegrees b ->
            Json.Encode.float b


latitudeDecoder : Json.Decode.Decoder Latitude
latitudeDecoder =
    Json.Decode.map LatitudeDegrees Json.Decode.float


type Longitude
    = LongitudeDegrees Float


longitudeEncoder : Longitude -> Json.Encode.Value
longitudeEncoder a =
    case a of
        LongitudeDegrees b ->
            Json.Encode.float b


longitudeDecoder : Json.Decode.Decoder Longitude
longitudeDecoder =
    Json.Decode.map LongitudeDegrees Json.Decode.float


type Elevation
    = ElevationMeters Float


elevationEncoder : Elevation -> Json.Encode.Value
elevationEncoder a =
    case a of
        ElevationMeters b ->
            Json.Encode.float b


elevationDecoder : Json.Decode.Decoder Elevation
elevationDecoder =
    Json.Decode.map ElevationMeters Json.Decode.float


type Direction
    = DirectionDegrees Int


directionEncoder : Direction -> Json.Encode.Value
directionEncoder a =
    case a of
        DirectionDegrees b ->
            Json.Encode.int b


directionDecoder : Json.Decode.Decoder Direction
directionDecoder =
    Json.Decode.map DirectionDegrees Json.Decode.int


type Length
    = LengthMeters Float


lengthEncoder : Length -> Json.Encode.Value
lengthEncoder a =
    case a of
        LengthMeters b ->
            Json.Encode.float b


lengthDecoder : Json.Decode.Decoder Length
lengthDecoder =
    Json.Decode.map LengthMeters Json.Decode.float


type alias NavPoint =
    { name : String
    , code : String
    , country : Maybe String
    , lat : Latitude
    , lon : Longitude
    , elev : Elevation
    , style : WaypointStyle
    , rwdir : Maybe Direction
    , rwlen : Maybe Length
    , freq : Maybe String
    , desc : String
    }


navPointEncoder : NavPoint -> Json.Encode.Value
navPointEncoder a =
    Json.Encode.object
        [ ( "name", Json.Encode.string a.name )
        , ( "code", Json.Encode.string a.code )
        , ( "country", Maybe.Extra.unwrap Json.Encode.null Json.Encode.string a.country )
        , ( "lat", latitudeEncoder a.lat )
        , ( "lon", longitudeEncoder a.lon )
        , ( "elev", elevationEncoder a.elev )
        , ( "style", waypointStyleEncoder a.style )
        , ( "rwdir", Maybe.Extra.unwrap Json.Encode.null directionEncoder a.rwdir )
        , ( "rwlen", Maybe.Extra.unwrap Json.Encode.null lengthEncoder a.rwlen )
        , ( "freq", Maybe.Extra.unwrap Json.Encode.null Json.Encode.string a.freq )
        , ( "desc", Json.Encode.string a.desc )
        ]


navPointDecoder : Json.Decode.Decoder NavPoint
navPointDecoder =
    Json.Decode.succeed NavPoint
        |> Json.Decode.Pipeline.required "name" Json.Decode.string
        |> Json.Decode.Pipeline.required "code" Json.Decode.string
        |> Json.Decode.Pipeline.required "country" (Json.Decode.nullable Json.Decode.string)
        |> Json.Decode.Pipeline.required "lat" latitudeDecoder
        |> Json.Decode.Pipeline.required "lon" longitudeDecoder
        |> Json.Decode.Pipeline.required "elev" elevationDecoder
        |> Json.Decode.Pipeline.required "style" waypointStyleDecoder
        |> Json.Decode.Pipeline.required "rwdir" (Json.Decode.nullable directionDecoder)
        |> Json.Decode.Pipeline.required "rwlen" (Json.Decode.nullable lengthDecoder)
        |> Json.Decode.Pipeline.required "freq" (Json.Decode.nullable Json.Decode.string)
        |> Json.Decode.Pipeline.required "desc" Json.Decode.string
