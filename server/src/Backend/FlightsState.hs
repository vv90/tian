module Backend.FlightsState where

import Aprs.AprsMessage (AprsMessage (glidernetId, source, symbol), getDeviceId)
import Aprs.AprsSymbol (AprsSymbol (..))
import Aprs.AprsSymbol qualified as AprsSymbol
import Aprs.GlidernetId (AircraftType)
import Aprs.GlidernetId qualified as GlidernetId
import Data.Aeson qualified as Aeson
import Data.HashMap.Strict as HM
import Data.Time (secondsToDiffTime)
import Generics.SOP qualified as SOP
import Geo
  ( Elevation,
    GeoPosition (..),
    GeoPosition3d (..),
    Latitude,
    Longitude,
    RecordedGeoPosition (..),
  )
import Glidernet.DeviceDatabase (DeviceInfo (..))
import Language.Haskell.To.Elm (HasElmDecoder, HasElmEncoder, HasElmType)
import Magic.ElmDeriving (ElmType)
import Relude

data FlightInformation = FlightInformation
  { deviceInfo :: Maybe DeviceInfo,
    aircraftType :: AircraftType
  }
  deriving stock (Show, Read, Eq, Generic)
  deriving anyclass (SOP.Generic, SOP.HasDatatypeInfo, Aeson.ToJSON, Aeson.FromJSON)
  deriving
    (HasElmType, HasElmEncoder Aeson.Value, HasElmDecoder Aeson.Value)
    via ElmType "Api.Types.FlightInformation" FlightInformation

data FlightPosition = FlightPosition
  { lat :: Latitude,
    lon :: Longitude,
    alt :: Elevation,
    timeSeconds :: Int
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (SOP.Generic, SOP.HasDatatypeInfo, Aeson.ToJSON, Aeson.FromJSON)
  deriving
    (HasElmType, HasElmEncoder Aeson.Value, HasElmDecoder Aeson.Value)
    via ElmType "Api.Types.FlightPosition" FlightPosition

toFlightPosition :: (GeoPosition3d a, RecordedGeoPosition a) => a -> FlightPosition
toFlightPosition p =
  FlightPosition
    { lat = latitude p,
      lon = longitude p,
      alt = altitude p,
      timeSeconds = round $ Geo.time p
    }

instance GeoPosition FlightPosition where
  latitude x = x.lat
  longitude x = x.lon

instance GeoPosition3d FlightPosition where
  altitude = alt

instance RecordedGeoPosition FlightPosition where
  time = secondsToDiffTime . fromIntegral . timeSeconds

aircraftTypeFromSymbol :: AprsSymbol -> AircraftType
aircraftTypeFromSymbol AprsSymbol.Glider = GlidernetId.Glider
aircraftTypeFromSymbol AprsSymbol.SmallPlane = GlidernetId.PistonAircraft
aircraftTypeFromSymbol AprsSymbol.LargePlane = GlidernetId.JetAircraft
aircraftTypeFromSymbol AprsSymbol.Helicopter = GlidernetId.Helicopter
aircraftTypeFromSymbol AprsSymbol.Parachute = GlidernetId.Parachute
aircraftTypeFromSymbol AprsSymbol.Balloon = GlidernetId.Balloon
aircraftTypeFromSymbol AprsSymbol.GroundVehicle = GlidernetId.Other
aircraftTypeFromSymbol AprsSymbol.StaticObject = GlidernetId.StaticObstacle
aircraftTypeFromSymbol AprsSymbol.Unknown = GlidernetId.Other

makeFlightInformation :: Maybe DeviceInfo -> AprsMessage -> FlightInformation
makeFlightInformation deviceInfo msg =
  FlightInformation
    { deviceInfo = deviceInfo,
      aircraftType = maybe (aircraftTypeFromSymbol msg.symbol) GlidernetId.senderType msg.glidernetId
    }

lookupFlightInformation :: HashMap Text DeviceInfo -> AprsMessage -> FlightInformation
lookupFlightInformation devicesDict msg =
  makeFlightInformation (HM.lookup (getDeviceId msg.source) devicesDict) msg
