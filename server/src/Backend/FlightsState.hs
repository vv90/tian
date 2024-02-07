module Backend.FlightsState where

import Aprs.AprsMessage (AprsMessage (..), getDeviceId)
import Aprs.AprsSymbol (AprsSymbol (..))
import Aprs.AprsSymbol qualified as AprsSymbol
import Aprs.GlidernetId (AircraftType)
import Aprs.GlidernetId qualified as GlidernetId
import Data.Aeson qualified as Aeson
import Data.HashMap.Strict as HM
import Data.Time (DiffTime)
import Geo
  ( Elevation,
    GeoPosition (..),
    GeoPosition3d (..),
    Latitude,
    Longitude,
    RecordedGeoPosition (..),
  )
import Glidernet.DeviceDatabase (DeviceInfo (..))
import Relude

data FlightInformation = FlightInformation
  { deviceInfo :: Maybe DeviceInfo,
    aircraftType :: AircraftType
  }
  deriving stock (Show, Eq)

data FlightPosition = FlightPosition
  { lat :: Latitude,
    lon :: Longitude,
    alt :: Elevation,
    time :: DiffTime
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (Aeson.ToJSON, Aeson.FromJSON)

toFlightPosition :: (GeoPosition3d a, RecordedGeoPosition a) => a -> FlightPosition
toFlightPosition p =
  FlightPosition
    { lat = latitude p,
      lon = longitude p,
      alt = altitude p,
      time = Geo.time p
    }

instance GeoPosition FlightPosition where
  latitude x = x.lat
  longitude x = x.lon

instance GeoPosition3d FlightPosition where
  altitude = alt

instance RecordedGeoPosition FlightPosition where
  time p = p.time

makeFlightInformation :: HashMap Text DeviceInfo -> AprsMessage -> FlightInformation
makeFlightInformation devicesDict msg =
  let aircraftTypeFromSymbol :: AprsSymbol -> AircraftType
      aircraftTypeFromSymbol AprsSymbol.Glider = GlidernetId.Glider
      aircraftTypeFromSymbol AprsSymbol.SmallPlane = GlidernetId.PistonAircraft
      aircraftTypeFromSymbol AprsSymbol.LargePlane = GlidernetId.JetAircraft
      aircraftTypeFromSymbol AprsSymbol.Helicopter = GlidernetId.Helicopter
      aircraftTypeFromSymbol AprsSymbol.Parachute = GlidernetId.Parachute
      aircraftTypeFromSymbol AprsSymbol.Balloon = GlidernetId.Balloon
      aircraftTypeFromSymbol AprsSymbol.GroundVehicle = GlidernetId.Other
      aircraftTypeFromSymbol AprsSymbol.StaticObject = GlidernetId.StaticObstacle
      aircraftTypeFromSymbol AprsSymbol.Unknown = GlidernetId.Other
   in FlightInformation
        { deviceInfo = HM.lookup (getDeviceId msg.source) devicesDict,
          aircraftType = maybe (aircraftTypeFromSymbol msg.symbol) GlidernetId.senderType msg.glidernetId
        }
