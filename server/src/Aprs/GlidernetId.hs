module Aprs.GlidernetId where

import Data.Bits (Bits (shiftR), (.&.))
import Numeric (readHex)
import Relude
import Text.Parsec (Parsec, alphaNum, many1, string)

-- Aircraft Category Type table:
-- {"cat_id":0b0001,"cat_name":"Gliders"},
-- {"cat_id":0b0010,"cat_name":"Planes"},
-- {"cat_id":0b0011,"cat_name":"Ultralights"}
-- {"cat_id":0b0100,"cat_name":"Helicopters"},
-- {"cat_id":0b0101,"cat_name":"Drones\/UAV"},
-- {"cat_id":0b0110,"cat_name":"Other"},
-- {"cat_id":0b0111,"cat_name":"Paragliders"},
-- {"cat_id":0b1000,"cat_name":"None"}

-- Hexadecimal value. Range: from 0 to F.
-- Aircraft types as assigned by FLARM:
-- 0 = (reserved)
-- 1 = glider/motor glider (turbo, self-launch, jet) / TMG
-- 2 = tow plane/tug plane
-- 3 = helicopter/gyrocopter/rotorcraft
-- 4 = skydiver, parachute (Do not use for drop plane!)
-- 5 = drop plane for skydivers
-- 6 = hang glider (hard)
-- 7 = paraglider (soft)
-- 8 = aircraft with reciprocating engine(s)
-- 9 = aircraft with jet/turboprop engine(s)
-- A = unknown
-- B = balloon (hot, gas, weather, static)
-- C = airship, blimp, zeppelin
-- D = unmanned aerial vehicle (UAV, RPAS, drone)
-- E = (reserved)
-- F = static obstacle

data AircraftType
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
  deriving stock (Show, Eq)

matchAircraftType :: (Num a, Eq a) => a -> AircraftType
matchAircraftType = \case
  0x0 -> UnknownAircraftType
  0x1 -> Glider
  0x2 -> TowPlane
  0x3 -> Helicopter
  0x4 -> Parachute
  0x5 -> DropPlane
  0x6 -> HangGlider
  0x7 -> ParaGlider
  0x8 -> PistonAircraft
  0x9 -> JetAircraft
  0xA -> UnknownAircraftType
  0xB -> Balloon
  0xC -> Airship
  0xD -> Drone
  0xE -> Other
  0xF -> StaticObstacle
  _ -> UnknownAircraftType

-- Address Type: 0b00 -> Unkown, 0b01 -> ICAO, 0b10 -> Flarm, 0b11 -> OGN tracker
data AddressType
  = AddressTypeUnknown
  | AddressTypeICAO
  | AddressTypeFlarm
  | AddressTypeOGN
  deriving stock (Show, Eq)

matchAddressType :: (Num a, Eq a) => a -> AddressType
matchAddressType = \case
  0b00 -> AddressTypeUnknown
  0b01 -> AddressTypeICAO
  0b10 -> AddressTypeFlarm
  0b11 -> AddressTypeOGN
  _ -> AddressTypeUnknown

data GlidernetIdInfo = GlidernetIdInfo
  { stealthMode :: Bool,
    noTracking :: Bool,
    senderType :: AircraftType,
    addressType :: AddressType,
    address :: Integer
  }
  deriving stock (Show, Eq)

glidernetIdParser :: Parsec ByteString () GlidernetIdInfo
glidernetIdParser = do
  void $ string "id"
  h <- readHex <$> many1 alphaNum

  case h of
    [(x, "")] -> pure $ decodeGlidernetId x
    _ -> fail "Failed to parse glidernet id"

-- toText <$> many1 alphaNum

-- The Glidernet ID is a string in the format:
-- idXXYYYYYY

-- where X and Y stand for hexadecimal digits. YYYYYY is the address.
-- XX encodes stealth mode S, no-tracking flag T, aircraft type tttt and address type aa as follows:
-- STttttaa

-- S, T, tttt, aa stand for 8 bits from most to least significant. Note that no messages with
-- the no-tracking set flag should ever appear on the public APRS network.
-- Sample: id02DF0A52
-- 0x06 = 0b00000110: sender details (2 digit hex number encoding 8 bits)
-- 0b0 = false: stealth mode boolean (should never be "1")
-- 0b0 = false: no-tracking boolean (must ignore if "1")
-- 0b0001 = 1: sender (aircraft) type ("GLIDER")
-- 0b10 = 2: address type ("FLARM")
-- DF0A52: sender address

decodeGlidernetId :: Integer -> GlidernetIdInfo
decodeGlidernetId gnId =
  let stealthMode = (gnId .&. 0x80000000) /= 0
      noTracking = (gnId .&. 0x40000000) /= 0
      senderType = (gnId .&. 0x3C000000) `shiftR` 26
      addressType = (gnId .&. 0x03000000) `shiftR` 24
      address = gnId .&. 0x00FFFFFF
   in GlidernetIdInfo
        stealthMode
        noTracking
        (matchAircraftType senderType)
        (matchAddressType addressType)
        address
