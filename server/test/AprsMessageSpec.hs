module AprsMessageSpec where

import Aprs.AprsMessage (AprsMessage (..), DeviceId (..), aprsMessageParser)
import Aprs.GlidernetId (AddressType (..), AircraftType (..), GlidernetIdInfo (..), glidernetIdParser)
import Geo (Latitude (..), Longitude (..), ddmTodd)
import Relude
import Test.Hspec
import Text.Parsec (parse)

spec :: Spec
spec = do
  context "glidernet parser" $ do
    it "FLARM / Glider" $ do
      let input = "id06DF0A52" :: ByteString
          result = parse glidernetIdParser "" input

      result
        `shouldBe` Right
          ( GlidernetIdInfo
              { stealthMode = False,
                noTracking = False,
                senderType = Glider,
                addressType = AddressTypeFlarm,
                address = 0xDF0A52
              }
          )

    it "ICAO / Piston" $ do
      let input = "id214B28B7" :: ByteString
          result = parse glidernetIdParser "" input

      result
        `shouldBe` Right
          ( GlidernetIdInfo
              { stealthMode = False,
                noTracking = False,
                senderType = PistonAircraft,
                addressType = AddressTypeICAO,
                address = 0x4B28B7
              }
          )

    it "ICAO / Jet" $ do
      let input = "id254BC883" :: ByteString
          result = parse glidernetIdParser "" input

      result
        `shouldBe` Right
          ( GlidernetIdInfo
              { stealthMode = False,
                noTracking = False,
                senderType = JetAircraft,
                addressType = AddressTypeICAO,
                address = 0x4BC883
              }
          )

    it "ICAO / Helicopter" $ do
      let input = "id0D484A94" :: ByteString
          result = parse glidernetIdParser "" input

      result
        `shouldBe` Right
          ( GlidernetIdInfo
              { stealthMode = False,
                noTracking = False,
                senderType = Helicopter,
                addressType = AddressTypeICAO,
                address = 0x484A94
              }
          )

  context "AprsMessage parser" $ do
    it "parses all" $ do
      let inputs =
            [ "ICA4B28B7>OGFLR,qAS,MUEN:/143013h4658.89N\\00710.87E^339/156/A=005870 !W04! id214B28B7 -157fpm -0.1rot 14.0dB -6.3kHz gps4x4",
              "ICA3E5C54>OGFLR,qAS,EDNE:/143008h4820.69N/00955.10E'206/011/A=001608 !W43! id093E5C54 +020fpm +1.2rot 28.5dB -2.6kHz gps10x6",
              "FLRD0230A>OGFLR,qAS,AVXEE42DB:/143013h4820.01N\\00828.89En000/000/A=002517 !W74! id3ED0230A +020fpm +0.0rot 5.2dB -9.6kHz gps7x8 +2.9dBm",
              "FLRDDDBD3>OGFLR,qAS,LFMF:/143015h4336.74N/00642.04E'000/000/A=000705 !W96! id06DDDBD3 -039fpm +0.0rot 14.2dB +9.0kHz gps5x14",
              "FLRD02313>OGFLR,qAS,lmr05:/143011h5333.10N\\00838.36En000/000/A=000374 !W65! id3ED02313 +020fpm +0.0rot 10.8dB -8.8kHz gps1x2",
              "FLRD023A8>OGFLR,qAS,lmr91:/143013h5047.56N\\00816.95En000/000/A=002185 !W83! id3ED023A8 -019fpm +0.0rot 3.8dB 5e -11.5kHz gps2x2",
              "ICA3939FC>OGNSKY,qAS,SafeSky:/143012h4305.91N/00007.98W'121/075/A=005685 !W35! id203939FC +003fpm gps4x1",
              "FLRD02326>OGFLR,qAS,EDPD:/143013h4840.42N\\01215.42En000/000/A=002021 !W14! id3ED02326 -039fpm +0.0rot 18.2dB -11.5kHz gps3x5",
              "ICAA197C0>OGFLR,qAS,Meiersbrg:/143013h5110.37N\\00655.22E^339/141/A=001979 !W19! id21A197C0 +158fpm -0.3rot 2.8dB 4e -13.4kHz gps1x2",
              "ICA3D1E2A>OGFLR,qAS,EDTX:/143012h4916.23N/01010.92E'338/093/A=003396 !W84! id053D1E2A +119fpm +0.0rot 12.5dB +7.9kHz gps1x1",
              "ICA3E6B87>OGFLR,qAS,EDTX:/143012h4907.05N/00947.53E'280/052/A=001401 !W10! id053E6B87 -633fpm +0.0rot 47.5dB -4.3kHz gps2x3",
              "ICA3E7AE1>OGFLR,qAS,EDTX:/143012h4852.28N/01012.48E'051/102/A=004371 !W71! id093E7AE1 +040fpm +0.1rot 12.0dB +8.0kHz gps4x5",
              "ICA3D3ED1>OGNSKY,qAS,SafeSky:/143014h5222.00N/00730.07E'277/085/A=000528 !W34! id203D3ED1 -045fpm gps5x8",
              "ICA4B1B11>OGFLR,qAS,Morrens:/143013h4631.68N\\00636.57E^201/074/A=002655 !W07! id214B1B11 +713fpm -0.2rot 13.8dB -10.6kHz gps2x3",
              "ICA4B28B7>OGFLR,qAS,Morrens:/143013h4658.89N\\00710.87E^339/156/A=005860 !W04! id214B28B7 -157fpm -0.1rot 6.5dB 1e -8.1kHz gps4x4",
              "ICA4B40A1>OGFLR,qAS,Morrens:/143013h4632.35N\\00636.93E^188/083/A=002307 !W49! id214B40A1 +634fpm -0.2rot 9.8dB -4.2kHz gps1x2",
              "FLRDDD591>OGFLR,qAS,LECI1:/143013h4236.83N/00041.60W'252/057/A=005496 !W67! id06DDD591 +634fpm +1.6rot 16.5dB -0.7kHz gps2x3",
              "ICA3D1139>OGFLR,qAS,MyTownOG:/143013h5300.31N/01032.90E'316/062/A=001959 !W56! id093D1139 -118fpm +0.1rot 4.8dB +2.3kHz gps2x4",
              "OGNFF0002>APRS,qAS,NAVITER:/193059h4613.98N/01421.83E'000/000/A=000000 !W47! id2AFF0002 +000fpm +0.0rot",
              "ICA4B47D2>OGFLR,qAS,LFMF:/193102h4336.89N/00641.98E'000/000/A=000745 !W01! id054B47D2 +000fpm +0.0rot 29.8dB -0.2kHz gps6x7",
              "ICA3C6586>OGADSB,qAS,EDFW:/193101h5011.21N/00904.52E^195/245/A=006113 !W68! id253C6586 -1344fpm FL057.70 A3:DLH981 Sq3445",
              "OGNFF0001>APRS,qAS,NAVITER:/193101h4613.98N/01421.83E'000/000/A=000000 !W47! id2AFF0001 +000fpm +0.0rot",
              "FLRDF163C>OGFLR,qAS,TuDelft:/193101h5201.92N/00422.77E'134/133/A=000971 !W38! id06DF163C +079fpm -0.6rot 11.5dB -4.0kHz gps1x1",
              "FLRDDE1FC>OGFLR,qAS,LECD:/193109h4223.18N/00152.11E'000/000/A=003698 !W74! id06DDE1FC +178fpm 0.0rot 22.8dB +0.5kHz gps12x18",
              "ICA4CA763>OGADSB,qAS,SpainAVX:/123014h4028.72N\\00243.41W^234/261/A=013100 id254CA763 -576fpm  0rot !W61! fnA3:RYR6CQ regEI-EFH modelB738",
              "ICA484556>OGADSB,qAS,SpainAVX:/123014h4038.02N\\00329.60W^042/225/A=006975 id25484556 +3456fpm  0rot !W98! fnA3:KLM18C regPH-BXY modelB738",
              "ICA347387>OGADSB,qAS,SpainAVX:/123014h4044.51N\\00339.80W^003/272/A=008925 id25347387 +2112fpm  0rot !W37! fnA5:IBE6403 regEC-NXD modelA359",
              "ICA346204>OGADSB,qAS,SpainAVX:/123014h4020.08N\\00249.30W^252/267/A=011700 id25346204 -1408fpm  0rot !W94! fnA3:IBE32YW regEC-NDN modelA20N",
              "ICA34750A>OGADSB,qAS,SpainAVX:/123014h4005.20N\\00349.24W^072/262/A=009825 id2534750A -1024fpm  0rot !W41! fnA3:AEA54VR regEC-OBJ modelB738",
              "ICA34754D>OGADSB,qAS,SpainAVX:/123014h4034.16N\\00335.53W^297/184/A=004075 id2534754D +1664fpm  0rot !W90! fnA5:IBE6659 regEC-OAY modelA359",
              "ICA34758C>OGADSB,qAS,SpainAVX:/123014h4009.37N\\00332.67W^052/240/A=006475 id2534758C -1152fpm  0rot !W75! fnA3:IBS3827",
              "ICA347307>OGADSB,qAS,SpainAVX:/123014h4012.19N\\00326.31W^041/243/A=004450 id25347307 -1408fpm  0rot !W42! fnA3:AEA52HD regEC-NUZ modelB738",
              "ICA34538E>OGADSB,qAS,SpainAVX:/123014h4017.77N\\00256.14W^256/273/A=009850 id2534538E -960fpm  0rot !W28! fnA3:AEA48XM regEC-MPG modelB738",
              "ICA347312>OGADSB,qAS,SpainAVX:/123014h4020.23N\\00325.55W^321/182/A=003650 id25347312 +000fpm  0rot !W49! fnA3:AEA76XR regEC-NVQ modelB738",
              "ICA34520B>OGADSB,qAS,SpainAVX:/123014h4027.01N\\00332.45W^322/143/A=001925 id2534520B -768fpm  0rot !W77! fnA5:IBE6460 regEC-MLB modelA332",
              "ICA010246>OGADSB,qAS,SpainAVX:/123014h4023.44N\\00327.14W^322/159/A=003625 id25010246 -960fpm  0rot !W97! fnA3:MSR753",
              "ICA346519>OGADSB,qAS,SpainAVX:/123014h4045.36N\\00402.09W^180/001/A=004525 id25346519 +064fpm  0rot !W75! fnA7:BOMBE01 regEC-NPV modelUNKW",
              "ICA347311>OGADSB,qAS,SpainAVX:/123014h4018.27N\\00401.21W^107/287/A=008600 id25347311 -832fpm  0rot !W26! fnA3:AEA7315 regEC-NVP modelB738",
              "ICA34554D>OGADSB,qAS,SpainAVX:/123014h4033.07N\\00403.75W^174/097/A=003675 id2534554D -064fpm  0rot !W97! fnA1:ECCZO regEC-CZO modelUNKW",
              "ICA4CA7B6>OGADSB,qAS,SpainAVX:/123014h4016.24N\\00412.06W^288/252/A=012450 id254CA7B6 -960fpm  0rot !W27! fnA3:RYR67VP regEI-EGA modelB738",
              "ICA3475D6>OGADSB,qAS,SpainAVX:/123014h4009.36N\\00411.66W^266/073/A=004200 id253475D6 -192fpm  0rot !W24! fnA1:QFY41",
              "ICA3463D7>OGADSB,qAS,SpainAVX:/123014h4013.98N\\00402.72W^293/066/A=002000 id253463D7 +384fpm  0rot !W78! fnA1:QFY83 regEC-NHV modelUNKW",
              "ICA3453CC>OGADSB,qAS,SpainAVX:/123014h4007.83N\\00342.14W^063/243/A=005675 id253453CC -1216fpm  0rot !W84! fnA3:AEA31QJ regEC-MQP modelB738",
              "ICA3474D5>OGADSB,qAS,SpainAVX:/123014h4052.39N\\00335.43W^316/099/A=003675 id253474D5 +000fpm  0rot !W85! fnA7:CUCO74",
              "ICA06A063>OGADSB,qAS,SpainAVX:/123014h4015.25N\\00308.72W^282/268/A=007175 id2506A063 -384fpm  0rot !W94! fnA5:QTR17B regA7-BAI modelB77W",
              "ICA4BC883>OGADSB,qAS,SpainAVX:/123014h4045.72N\\00314.42W^069/361/A=012425 id254BC883 +2496fpm  0rot !W41! fnA3:PGT1100 regTC-RDC modelA21N",
              "ICA3532C4>OGADSB,qAS,SpainAVX:/123013h4146.03N\\00048.44W^162/273/A=002100 id253532C4 -064fpm  0rot !W62! fnA4:MAMUT76 regT.23-06 modelUNKW",
              "ICA345218>OGADSB,qAS,SpainAVX:/123014h4017.05N\\00319.51W^292/236/A=006550 id25345218 -768fpm  0rot !W38! fnA3:AEA54QN regEC-MKL modelB738",
              "ICA44CDC2>OGADSB,qAS,SpainAVX:/123014h4031.15N\\00333.55W^000/163/A=001800 id2544CDC2 +2624fpm  0rot !W62! fnA3:BEL7LX regOO-SNB modelA320",
              "ICA3E6E0A>APRS,qAS,Sustenpas:/131325h4644.26N/00818.39E'148/068/A=015514 !W27! id053E6E0A +040fpm -0.7rot 6.2dB 0e +4.4kHz",
              "ICA44A831>OGADSB,qAS,AVX910:/132016h5043.34N/00439.21E^143/285/A=010727 !W09! id2544A831 A3:JAF8FT FL114.16 +3520fpm "
            ] ::
              [ByteString]
          results = traverse (parse aprsMessageParser "") inputs

      void results `shouldBe` Right ()

    it "parses msg 1" $ do
      let input = "FLRD0095F>OGFLR,qAS,LFNE:/160721h4337.14N/00507.75E'161/061/A=001204 !W82! id06D0095F -157fpm +0.1rot 10.2dB -0.5kHz gps1x2" :: ByteString
          result = parse aprsMessageParser "" input

      source <$> result `shouldBe` Right (DeviceId "FLRD0095F")
      lat <$> result `shouldBe` Right (LatitudeDegrees $ ddmTodd 43 37.148)
      lon <$> result `shouldBe` Right (LongitudeDegrees $ ddmTodd 5 7.752)
