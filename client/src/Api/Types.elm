module Api.Types exposing
    ( AircraftType(..)
    , DeviceId(..)
    , DeviceInfo
    , Direction(..)
    , Distance(..)
    , Elevation(..)
    , ElevationPointsTile
    , Entity
    , FlightInformation
    , FlightPosition
    , FlightTask
    , GeoPoint
    , Latitude(..)
    , Longitude(..)
    , NameMatch
    , NavPoint
    , ProgressPoint
    , TaskFinish(..)
    , TaskProgress
    , TaskStart(..)
    , Turnpoint(..)
    , WaypointStyle(..)
    , aircraftTypeDecoder
    , aircraftTypeEncoder
    , deviceIdDecoder
    , deviceIdEncoder
    , deviceInfoDecoder
    , deviceInfoEncoder
    , directionDecoder
    , directionEncoder
    , distanceDecoder
    , distanceEncoder
    , elevationDecoder
    , elevationEncoder
    , elevationPointsTileDecoder
    , elevationPointsTileEncoder
    , entityDecoder
    , entityEncoder
    , flightInformationDecoder
    , flightInformationEncoder
    , flightPositionDecoder
    , flightPositionEncoder
    , flightTaskDecoder
    , flightTaskEncoder
    , geoPointDecoder
    , geoPointEncoder
    , latitudeDecoder
    , latitudeEncoder
    , longitudeDecoder
    , longitudeEncoder
    , nameMatchDecoder
    , nameMatchEncoder
    , navPointDecoder
    , navPointEncoder
    , progressPointDecoder
    , progressPointEncoder
    , taskFinishDecoder
    , taskFinishEncoder
    , taskProgressDecoder
    , taskProgressEncoder
    , taskStartDecoder
    , taskStartEncoder
    , turnpointDecoder
    , turnpointEncoder
    , waypointStyleDecoder
    , waypointStyleEncoder
    )

import Array
import Iso8601
import Json.Decode
import Json.Decode.Pipeline
import Json.Encode
import Maybe.Extra
import Time


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


type Distance
    = DistanceMeters Float


distanceEncoder : Distance -> Json.Encode.Value
distanceEncoder a =
    case a of
        DistanceMeters b ->
            Json.Encode.float b


distanceDecoder : Json.Decode.Decoder Distance
distanceDecoder =
    Json.Decode.map DistanceMeters Json.Decode.float


type alias NavPoint =
    { name : String
    , code : String
    , country : Maybe String
    , lat : Latitude
    , lon : Longitude
    , elev : Elevation
    , style : WaypointStyle
    , rwdir : Maybe Direction
    , rwlen : Maybe Distance
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
        , ( "rwlen", Maybe.Extra.unwrap Json.Encode.null distanceEncoder a.rwlen )
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
        |> Json.Decode.Pipeline.required "rwlen" (Json.Decode.nullable distanceDecoder)
        |> Json.Decode.Pipeline.required "freq" (Json.Decode.nullable Json.Decode.string)
        |> Json.Decode.Pipeline.required "desc" Json.Decode.string


type Turnpoint
    = TurnpointCylinder Float


turnpointEncoder : Turnpoint -> Json.Encode.Value
turnpointEncoder a =
    case a of
        TurnpointCylinder b ->
            Json.Encode.float b


turnpointDecoder : Json.Decode.Decoder Turnpoint
turnpointDecoder =
    Json.Decode.map TurnpointCylinder Json.Decode.float


type TaskStart
    = StartLine Float


taskStartEncoder : TaskStart -> Json.Encode.Value
taskStartEncoder a =
    case a of
        StartLine b ->
            Json.Encode.float b


taskStartDecoder : Json.Decode.Decoder TaskStart
taskStartDecoder =
    Json.Decode.map StartLine Json.Decode.float


type TaskFinish
    = FinishLine Float
    | FinishCylinder Float


taskFinishEncoder : TaskFinish -> Json.Encode.Value
taskFinishEncoder a =
    case a of
        FinishLine b ->
            Json.Encode.object
                [ ( "tag", Json.Encode.string "FinishLine" )
                , ( "contents", Json.Encode.float b )
                ]

        FinishCylinder b ->
            Json.Encode.object
                [ ( "tag", Json.Encode.string "FinishCylinder" )
                , ( "contents", Json.Encode.float b )
                ]


taskFinishDecoder : Json.Decode.Decoder TaskFinish
taskFinishDecoder =
    Json.Decode.field "tag" Json.Decode.string
        |> Json.Decode.andThen
            (\a ->
                case a of
                    "FinishLine" ->
                        Json.Decode.succeed FinishLine
                            |> Json.Decode.Pipeline.required "contents" Json.Decode.float

                    "FinishCylinder" ->
                        Json.Decode.succeed FinishCylinder
                            |> Json.Decode.Pipeline.required "contents" Json.Decode.float

                    _ ->
                        Json.Decode.fail "No matching constructor"
            )


type alias FlightTask =
    { start : ( NavPoint, TaskStart )
    , turnpoints : List ( NavPoint, Turnpoint )
    , finish : ( NavPoint, TaskFinish )
    }


flightTaskEncoder : FlightTask -> Json.Encode.Value
flightTaskEncoder a =
    Json.Encode.object
        [ ( "start"
          , case a.start of
                ( b, c ) ->
                    Json.Encode.list identity [ navPointEncoder b, taskStartEncoder c ]
          )
        , ( "turnpoints"
          , Json.Encode.list
                (\b ->
                    case b of
                        ( c, d ) ->
                            Json.Encode.list identity
                                [ navPointEncoder c
                                , turnpointEncoder d
                                ]
                )
                a.turnpoints
          )
        , ( "finish"
          , case a.finish of
                ( b, c ) ->
                    Json.Encode.list identity
                        [ navPointEncoder b
                        , taskFinishEncoder c
                        ]
          )
        ]


flightTaskDecoder : Json.Decode.Decoder FlightTask
flightTaskDecoder =
    Json.Decode.succeed FlightTask
        |> Json.Decode.Pipeline.required "start" (Json.Decode.map2 Tuple.pair (Json.Decode.index 0 navPointDecoder) (Json.Decode.index 1 taskStartDecoder))
        |> Json.Decode.Pipeline.required "turnpoints" (Json.Decode.list (Json.Decode.map2 Tuple.pair (Json.Decode.index 0 navPointDecoder) (Json.Decode.index 1 turnpointDecoder)))
        |> Json.Decode.Pipeline.required "finish" (Json.Decode.map2 Tuple.pair (Json.Decode.index 0 navPointDecoder) (Json.Decode.index 1 taskFinishDecoder))


type alias Entity a b =
    { key : a, entity : b }


entityEncoder : (a -> Json.Encode.Value) -> (b -> Json.Encode.Value) -> Entity a b -> Json.Encode.Value
entityEncoder a b c =
    Json.Encode.object [ ( "key", a c.key ), ( "entity", b c.entity ) ]


entityDecoder : Json.Decode.Decoder a -> Json.Decode.Decoder b -> Json.Decode.Decoder (Entity a b)
entityDecoder a b =
    Json.Decode.succeed Entity
        |> Json.Decode.Pipeline.required "key" a
        |> Json.Decode.Pipeline.required "entity" b


type alias ProgressPoint =
    { time : Int
    , lat : Latitude
    , lon : Longitude
    , altitude : Elevation
    , target : Maybe String
    , distance : Float
    , speed : Maybe Float
    }


progressPointEncoder : ProgressPoint -> Json.Encode.Value
progressPointEncoder a =
    Json.Encode.object
        [ ( "time", Json.Encode.int a.time )
        , ( "lat", latitudeEncoder a.lat )
        , ( "lon", longitudeEncoder a.lon )
        , ( "altitude", elevationEncoder a.altitude )
        , ( "target", Maybe.Extra.unwrap Json.Encode.null Json.Encode.string a.target )
        , ( "distance", Json.Encode.float a.distance )
        , ( "speed", Maybe.Extra.unwrap Json.Encode.null Json.Encode.float a.speed )
        ]


progressPointDecoder : Json.Decode.Decoder ProgressPoint
progressPointDecoder =
    Json.Decode.succeed ProgressPoint
        |> Json.Decode.Pipeline.required "time" Json.Decode.int
        |> Json.Decode.Pipeline.required "lat" latitudeDecoder
        |> Json.Decode.Pipeline.required "lon" longitudeDecoder
        |> Json.Decode.Pipeline.required "altitude" elevationDecoder
        |> Json.Decode.Pipeline.required "target" (Json.Decode.nullable Json.Decode.string)
        |> Json.Decode.Pipeline.required "distance" Json.Decode.float
        |> Json.Decode.Pipeline.required "speed" (Json.Decode.nullable Json.Decode.float)


type alias TaskProgress =
    { taskId : Int
    , date : Time.Posix
    , compId : String
    , points : List ProgressPoint
    , legs : List ( Latitude, Longitude )
    }


taskProgressEncoder : TaskProgress -> Json.Encode.Value
taskProgressEncoder a =
    Json.Encode.object
        [ ( "taskId", Json.Encode.int a.taskId )
        , ( "date", Iso8601.encode a.date )
        , ( "compId", Json.Encode.string a.compId )
        , ( "points", Json.Encode.list progressPointEncoder a.points )
        , ( "legs"
          , Json.Encode.list
                (\b ->
                    case b of
                        ( c, d ) ->
                            Json.Encode.list identity
                                [ latitudeEncoder c
                                , longitudeEncoder d
                                ]
                )
                a.legs
          )
        ]


taskProgressDecoder : Json.Decode.Decoder TaskProgress
taskProgressDecoder =
    Json.Decode.succeed TaskProgress
        |> Json.Decode.Pipeline.required "taskId" Json.Decode.int
        |> Json.Decode.Pipeline.required "date" Iso8601.decoder
        |> Json.Decode.Pipeline.required "compId" Json.Decode.string
        |> Json.Decode.Pipeline.required "points" (Json.Decode.list progressPointDecoder)
        |> Json.Decode.Pipeline.required "legs" (Json.Decode.list (Json.Decode.map2 Tuple.pair (Json.Decode.index 0 latitudeDecoder) (Json.Decode.index 1 longitudeDecoder)))


type alias NameMatch =
    { compId : String, name : String }


nameMatchEncoder : NameMatch -> Json.Encode.Value
nameMatchEncoder a =
    Json.Encode.object
        [ ( "compId", Json.Encode.string a.compId )
        , ( "name", Json.Encode.string a.name )
        ]


nameMatchDecoder : Json.Decode.Decoder NameMatch
nameMatchDecoder =
    Json.Decode.succeed NameMatch
        |> Json.Decode.Pipeline.required "compId" Json.Decode.string
        |> Json.Decode.Pipeline.required "name" Json.Decode.string


type alias GeoPoint =
    { lat : Latitude, lon : Longitude }


geoPointEncoder : GeoPoint -> Json.Encode.Value
geoPointEncoder a =
    Json.Encode.object
        [ ( "lat", latitudeEncoder a.lat )
        , ( "lon", longitudeEncoder a.lon )
        ]


geoPointDecoder : Json.Decode.Decoder GeoPoint
geoPointDecoder =
    Json.Decode.succeed GeoPoint
        |> Json.Decode.Pipeline.required "lat" latitudeDecoder
        |> Json.Decode.Pipeline.required "lon" longitudeDecoder


type alias ElevationPointsTile =
    { origin : GeoPoint
    , latStep : Latitude
    , lonStep : Longitude
    , rowLength : Int
    , elevations : Array.Array Int
    }


elevationPointsTileEncoder : ElevationPointsTile -> Json.Encode.Value
elevationPointsTileEncoder a =
    Json.Encode.object
        [ ( "origin", geoPointEncoder a.origin )
        , ( "latStep", latitudeEncoder a.latStep )
        , ( "lonStep", longitudeEncoder a.lonStep )
        , ( "rowLength", Json.Encode.int a.rowLength )
        , ( "elevations", Json.Encode.array Json.Encode.int a.elevations )
        ]


elevationPointsTileDecoder : Json.Decode.Decoder ElevationPointsTile
elevationPointsTileDecoder =
    Json.Decode.succeed ElevationPointsTile
        |> Json.Decode.Pipeline.required "origin" geoPointDecoder
        |> Json.Decode.Pipeline.required "latStep" latitudeDecoder
        |> Json.Decode.Pipeline.required "lonStep" longitudeDecoder
        |> Json.Decode.Pipeline.required "rowLength" Json.Decode.int
        |> Json.Decode.Pipeline.required "elevations" (Json.Decode.array Json.Decode.int)


type alias DeviceInfo =
    { deviceType : String
    , deviceId : String
    , aircraftModel : Maybe String
    , registration : Maybe String
    , competitionNumber : Maybe String
    , tracked : Bool
    , identified : Bool
    }


deviceInfoEncoder : DeviceInfo -> Json.Encode.Value
deviceInfoEncoder a =
    Json.Encode.object
        [ ( "deviceType", Json.Encode.string a.deviceType )
        , ( "deviceId", Json.Encode.string a.deviceId )
        , ( "aircraftModel", Maybe.Extra.unwrap Json.Encode.null Json.Encode.string a.aircraftModel )
        , ( "registration", Maybe.Extra.unwrap Json.Encode.null Json.Encode.string a.registration )
        , ( "competitionNumber", Maybe.Extra.unwrap Json.Encode.null Json.Encode.string a.competitionNumber )
        , ( "tracked", Json.Encode.bool a.tracked )
        , ( "identified", Json.Encode.bool a.identified )
        ]


deviceInfoDecoder : Json.Decode.Decoder DeviceInfo
deviceInfoDecoder =
    Json.Decode.succeed DeviceInfo
        |> Json.Decode.Pipeline.required "deviceType" Json.Decode.string
        |> Json.Decode.Pipeline.required "deviceId" Json.Decode.string
        |> Json.Decode.Pipeline.required "aircraftModel" (Json.Decode.nullable Json.Decode.string)
        |> Json.Decode.Pipeline.required "registration" (Json.Decode.nullable Json.Decode.string)
        |> Json.Decode.Pipeline.required "competitionNumber" (Json.Decode.nullable Json.Decode.string)
        |> Json.Decode.Pipeline.required "tracked" Json.Decode.bool
        |> Json.Decode.Pipeline.required "identified" Json.Decode.bool


type alias FlightPosition =
    { lat : Latitude, lon : Longitude, alt : Elevation, timeSeconds : Int }


flightPositionEncoder : FlightPosition -> Json.Encode.Value
flightPositionEncoder a =
    Json.Encode.object
        [ ( "lat", latitudeEncoder a.lat )
        , ( "lon", longitudeEncoder a.lon )
        , ( "alt", elevationEncoder a.alt )
        , ( "timeSeconds", Json.Encode.int a.timeSeconds )
        ]


flightPositionDecoder : Json.Decode.Decoder FlightPosition
flightPositionDecoder =
    Json.Decode.succeed FlightPosition
        |> Json.Decode.Pipeline.required "lat" latitudeDecoder
        |> Json.Decode.Pipeline.required "lon" longitudeDecoder
        |> Json.Decode.Pipeline.required "alt" elevationDecoder
        |> Json.Decode.Pipeline.required "timeSeconds" Json.Decode.int


type alias FlightInformation =
    { deviceInfo : Maybe DeviceInfo, aircraftType : AircraftType }


flightInformationEncoder : FlightInformation -> Json.Encode.Value
flightInformationEncoder a =
    Json.Encode.object
        [ ( "deviceInfo", Maybe.Extra.unwrap Json.Encode.null deviceInfoEncoder a.deviceInfo )
        , ( "aircraftType", aircraftTypeEncoder a.aircraftType )
        ]


flightInformationDecoder : Json.Decode.Decoder FlightInformation
flightInformationDecoder =
    Json.Decode.succeed FlightInformation
        |> Json.Decode.Pipeline.required "deviceInfo" (Json.Decode.nullable deviceInfoDecoder)
        |> Json.Decode.Pipeline.required "aircraftType" aircraftTypeDecoder


type AircraftType
    = Glider
    | TowPlane
    | Helicopter
    | Parachute
    | DropPlane
    | HangGlider
    | ParaGlider
    | PistonAircraft
    | JetAircraft
    | UnknownAircraftType
    | Balloon
    | Airship
    | Drone
    | Other
    | StaticObstacle


aircraftTypeEncoder : AircraftType -> Json.Encode.Value
aircraftTypeEncoder a =
    case a of
        Glider ->
            Json.Encode.string "Glider"

        TowPlane ->
            Json.Encode.string "TowPlane"

        Helicopter ->
            Json.Encode.string "Helicopter"

        Parachute ->
            Json.Encode.string "Parachute"

        DropPlane ->
            Json.Encode.string "DropPlane"

        HangGlider ->
            Json.Encode.string "HangGlider"

        ParaGlider ->
            Json.Encode.string "ParaGlider"

        PistonAircraft ->
            Json.Encode.string "PistonAircraft"

        JetAircraft ->
            Json.Encode.string "JetAircraft"

        UnknownAircraftType ->
            Json.Encode.string "UnknownAircraftType"

        Balloon ->
            Json.Encode.string "Balloon"

        Airship ->
            Json.Encode.string "Airship"

        Drone ->
            Json.Encode.string "Drone"

        Other ->
            Json.Encode.string "Other"

        StaticObstacle ->
            Json.Encode.string "StaticObstacle"


aircraftTypeDecoder : Json.Decode.Decoder AircraftType
aircraftTypeDecoder =
    Json.Decode.string
        |> Json.Decode.andThen
            (\a ->
                case a of
                    "Glider" ->
                        Json.Decode.succeed Glider

                    "TowPlane" ->
                        Json.Decode.succeed TowPlane

                    "Helicopter" ->
                        Json.Decode.succeed Helicopter

                    "Parachute" ->
                        Json.Decode.succeed Parachute

                    "DropPlane" ->
                        Json.Decode.succeed DropPlane

                    "HangGlider" ->
                        Json.Decode.succeed HangGlider

                    "ParaGlider" ->
                        Json.Decode.succeed ParaGlider

                    "PistonAircraft" ->
                        Json.Decode.succeed PistonAircraft

                    "JetAircraft" ->
                        Json.Decode.succeed JetAircraft

                    "UnknownAircraftType" ->
                        Json.Decode.succeed UnknownAircraftType

                    "Balloon" ->
                        Json.Decode.succeed Balloon

                    "Airship" ->
                        Json.Decode.succeed Airship

                    "Drone" ->
                        Json.Decode.succeed Drone

                    "Other" ->
                        Json.Decode.succeed Other

                    "StaticObstacle" ->
                        Json.Decode.succeed StaticObstacle

                    _ ->
                        Json.Decode.fail "No matching constructor"
            )


type DeviceId
    = DeviceId String


deviceIdEncoder : DeviceId -> Json.Encode.Value
deviceIdEncoder a =
    case a of
        DeviceId b ->
            Json.Encode.string b


deviceIdDecoder : Json.Decode.Decoder DeviceId
deviceIdDecoder =
    Json.Decode.map DeviceId Json.Decode.string
