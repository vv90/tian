module DeviceDatabaseSpec where

import Glidernet.DeviceDatabase (DeviceInfo (..), DeviceType (..), deviceDatabaseParser, deviceInfoParser)
import Relude
import Test.Hspec
import Text.Parsec (parse)

-- #DEVICE_TYPE,DEVICE_ID,AIRCRAFT_MODEL,REGISTRATION,CN,TRACKED,IDENTIFIED
-- 'F','000000','HPH 304CZ-17','OK-7777','KN','Y','Y'
-- 'F','000114','','','','N','N'

spec :: Spec
spec = context "DeviceInfo parser" $ do
  it "parses valid input 1" $ do
    let cases :: (ByteString, DeviceInfo)
        cases =
          -- #DEVICE_TYPE,DEVICE_ID,AIRCRAFT_MODEL,REGISTRATION,CN,TRACKED,IDENTIFIED
          ( "'F','000000','HPH 304CZ-17','OK-7777','KN','Y','Y'\n",
            DeviceInfo
              { deviceType = Flarm,
                deviceId = "000000",
                aircraftModel = Just "HPH 304CZ-17",
                registration = Just "OK-7777",
                competitionNumber = Just "KN",
                tracked = True,
                identified = True
              }
          )
    parse deviceInfoParser "" (fst cases) `shouldBe` Right (snd cases)

  it "parses valid input 2" $ do
    let cases :: (ByteString, DeviceInfo)
        cases =
          ( "'F','000114','','','','N','N'\n",
            DeviceInfo
              { deviceType = Flarm,
                deviceId = "000114",
                aircraftModel = Nothing,
                registration = Nothing,
                competitionNumber = Nothing,
                tracked = False,
                identified = False
              }
          )
    parse deviceInfoParser "" (fst cases) `shouldBe` Right (snd cases)

  it "fails on invalid inputs" $ do
    let cases :: [ByteString]
        cases =
          -- #DEVICE_TYPE,DEVICE_ID,AIRCRAFT_MODEL,REGISTRATION,CN,TRACKED,IDENTIFIED
          [ "'F','000000''HPH 304CZ-17','OK-7777','KN','Y','Y'\n", -- missing comma
            "'F','000000','HPH 304CZ-17','OK-7777,'KN','Y','Y'\n", -- missing single quote
            "'F','000000','HPH 304CZ-17','OK-7777','KN','Y'\n", -- not enough items
            "'F','000000','HPH 304CZ-17','OK-7777','KN','Y','Y','Y'\n", -- too many items
            "'F','000000','HPH 304CZ-17','OK-7777','KN','Y','Y'", -- no newline at the end
            "'F','','HPH 304CZ-17','OK-7777','KN','Y','Y'\n", -- empty DEVICE_ID
            "'F','000000','HPH 304CZ-17','OK-7777','KN','Y','E'\n", -- invalid IDENTIFIED
            "'F','000000','HPH 304CZ-17','OK-7777','KN','','Y'\n" -- empty TRACKED
          ]
        results = parse deviceInfoParser "" <$> cases
    all isLeft results `shouldBe` True

  it "parses device database" $ do
    let databaseString :: ByteString
        databaseString = "#DEVICE_TYPE,DEVICE_ID,AIRCRAFT_MODEL,REGISTRATION,CN,TRACKED,IDENTIFIED\n'F','000000','HPH 304CZ-17','OK-7777','KN','Y','Y'\n'O','000001','Drone/UAV','aerobot','','Y','Y'\n'F','000002','LS-6 18','OY-XRG','G2','Y','Y'\n'F','000010','Unknown','D-EEAC','AC','Y','Y'\n'F','000011','Skylane Airlony','D-MTEW','EW','Y','Y'\n'F','000013','PA-28','D-EZIP','IP','Y','Y'\n'O','000015','Paraglider','36445','','Y','Y'\n'F','000037','','','','Y','N'\n'F','000040','Paraglider','','','Y','Y'\n'F','000063','','','','Y','N'\n'F','0000FD','Taurus','F-JRDN','DN','Y','Y'\n'O','000111','Parachute','I---AM','AM','Y','Y'\n'O','000112','Motorplane','','','Y','Y'\n'F','000114','','','','N','N'\n'O','000115','Ultralight','D-MDAJ','','Y','Y'\n'F','000150','Ultralight','D-MSKX','','Y','Y'\n'F','000176','Balloon','MCH-1','','Y','Y'\n'F','000195','Drone/UAV','AIRTEC','ATS','Y','Y'\n'F','000203','Drone/UAV','SRZ2000','','Y','Y'\n"
    isRight (parse deviceDatabaseParser "" databaseString) `shouldBe` True
