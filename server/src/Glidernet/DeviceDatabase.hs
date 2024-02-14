module Glidernet.DeviceDatabase where

import Data.Aeson qualified as Aeson
import Generics.SOP qualified as SOP
import Language.Haskell.To.Elm (HasElmDecoder, HasElmEncoder, HasElmType)
import Magic.ElmDeriving (ElmType)
import Relude
import Text.Parsec (Parsec, between, char, choice, endOfLine, many1, noneOf, optionMaybe)

-- #DEVICE_TYPE,DEVICE_ID,AIRCRAFT_MODEL,REGISTRATION,CN,TRACKED,IDENTIFIED
-- 'F','000000','HPH 304CZ-17','OK-7777','KN','Y','Y'

data DeviceType
  = Flarm
  | OGN
  | ICAO
  deriving stock (Show, Read, Eq, Generic)
  deriving anyclass (SOP.Generic, SOP.HasDatatypeInfo, Aeson.ToJSON, Aeson.FromJSON)
  deriving
    (HasElmType, HasElmEncoder Aeson.Value, HasElmDecoder Aeson.Value)
    via ElmType "Api.Types.DeviceType" DeviceType

data DeviceInfo = DeviceInfo
  { deviceType :: DeviceType,
    deviceId :: Text,
    aircraftModel :: Maybe Text,
    registration :: Maybe Text,
    competitionNumber :: Maybe Text,
    tracked :: Bool,
    identified :: Bool
  }
  deriving stock (Show, Read, Eq, Generic)
  deriving anyclass (SOP.Generic, SOP.HasDatatypeInfo, Aeson.ToJSON, Aeson.FromJSON)
  deriving
    (HasElmType, HasElmEncoder Aeson.Value, HasElmDecoder Aeson.Value)
    via ElmType "Api.Types.DeviceInfo" DeviceInfo

boolParser :: Parsec ByteString () Bool
boolParser =
  choice
    [ True <$ char 'Y',
      False <$ char 'N'
    ]

itemParser :: Parsec ByteString () a -> Parsec ByteString () a
itemParser =
  between (char '\'') (char '\'')

deviceTypeParser :: Parsec ByteString () DeviceType
deviceTypeParser =
  choice
    [ Flarm <$ char 'F',
      OGN <$ char 'O',
      ICAO <$ char 'I'
    ]

deviceInfoParser :: Parsec ByteString () DeviceInfo
deviceInfoParser =
  let nonEmptyTextParser :: Parsec ByteString () Text
      nonEmptyTextParser = toText <$> many1 (noneOf ['\'', ','])
   in DeviceInfo
        <$> itemParser deviceTypeParser
        <* char ','
        <*> itemParser nonEmptyTextParser
        <* char ','
        <*> itemParser (optionMaybe nonEmptyTextParser)
        <* char ','
        <*> itemParser (optionMaybe nonEmptyTextParser)
        <* char ','
        <*> itemParser (optionMaybe nonEmptyTextParser)
        <* char ','
        <*> itemParser boolParser
        <* char ','
        <*> itemParser boolParser
        <* endOfLine

commentParser :: Parsec ByteString () ()
commentParser =
  (char '#' *> many (noneOf ['\n']) *> char '\n') $> ()

deviceDatabaseParser :: Parsec ByteString () [DeviceInfo]
deviceDatabaseParser =
  commentParser *> many deviceInfoParser
