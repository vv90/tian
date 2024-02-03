module AprsMessageSpec where

import Aprs.AprsMessage (AprsMessage (..), aprsMessageParser)
import Relude
import Test.Hspec
import Text.Parsec (parse)

spec :: Spec
spec = do
  context "AprsMessage parser" $ do
    it "parses all" $ do
      let inputs =
            [ "ICA4B28B7>OGFLR,qAS,MUEN:/143013h4658.89N\\00710.87E^339/156/A=005870 !W04! id214B28B7 -157fpm -0.1rot 14.0dB -6.3kHz gps4x4\n",
              "ICA3E5C54>OGFLR,qAS,EDNE:/143008h4820.69N/00955.10E'206/011/A=001608 !W43! id093E5C54 +020fpm +1.2rot 28.5dB -2.6kHz gps10x6\n",
              "FLRD0230A>OGFLR,qAS,AVXEE42DB:/143013h4820.01N\\00828.89En000/000/A=002517 !W74! id3ED0230A +020fpm +0.0rot 5.2dB -9.6kHz gps7x8 +2.9dBm\n",
              "FLRDDDBD3>OGFLR,qAS,LFMF:/143015h4336.74N/00642.04E'000/000/A=000705 !W96! id06DDDBD3 -039fpm +0.0rot 14.2dB +9.0kHz gps5x14\n",
              "FLRD02313>OGFLR,qAS,lmr05:/143011h5333.10N\\00838.36En000/000/A=000374 !W65! id3ED02313 +020fpm +0.0rot 10.8dB -8.8kHz gps1x2\n",
              "FLRD023A8>OGFLR,qAS,lmr91:/143013h5047.56N\\00816.95En000/000/A=002185 !W83! id3ED023A8 -019fpm +0.0rot 3.8dB 5e -11.5kHz gps2x2\n",
              "ICA3939FC>OGNSKY,qAS,SafeSky:/143012h4305.91N/00007.98W'121/075/A=005685 !W35! id203939FC +003fpm gps4x1\n",
              "FLRD02326>OGFLR,qAS,EDPD:/143013h4840.42N\\01215.42En000/000/A=002021 !W14! id3ED02326 -039fpm +0.0rot 18.2dB -11.5kHz gps3x5\n",
              "ICAA197C0>OGFLR,qAS,Meiersbrg:/143013h5110.37N\\00655.22E^339/141/A=001979 !W19! id21A197C0 +158fpm -0.3rot 2.8dB 4e -13.4kHz gps1x2\n",
              "ICA3D1E2A>OGFLR,qAS,EDTX:/143012h4916.23N/01010.92E'338/093/A=003396 !W84! id053D1E2A +119fpm +0.0rot 12.5dB +7.9kHz gps1x1\n",
              "ICA3E6B87>OGFLR,qAS,EDTX:/143012h4907.05N/00947.53E'280/052/A=001401 !W10! id053E6B87 -633fpm +0.0rot 47.5dB -4.3kHz gps2x3\n",
              "ICA3E7AE1>OGFLR,qAS,EDTX:/143012h4852.28N/01012.48E'051/102/A=004371 !W71! id093E7AE1 +040fpm +0.1rot 12.0dB +8.0kHz gps4x5\n",
              "ICA3D3ED1>OGNSKY,qAS,SafeSky:/143014h5222.00N/00730.07E'277/085/A=000528 !W34! id203D3ED1 -045fpm gps5x8\n",
              "ICA4B1B11>OGFLR,qAS,Morrens:/143013h4631.68N\\00636.57E^201/074/A=002655 !W07! id214B1B11 +713fpm -0.2rot 13.8dB -10.6kHz gps2x3\n",
              "ICA4B28B7>OGFLR,qAS,Morrens:/143013h4658.89N\\00710.87E^339/156/A=005860 !W04! id214B28B7 -157fpm -0.1rot 6.5dB 1e -8.1kHz gps4x4\n",
              "ICA4B40A1>OGFLR,qAS,Morrens:/143013h4632.35N\\00636.93E^188/083/A=002307 !W49! id214B40A1 +634fpm -0.2rot 9.8dB -4.2kHz gps1x2\n",
              "FLRDDD591>OGFLR,qAS,LECI1:/143013h4236.83N/00041.60W'252/057/A=005496 !W67! id06DDD591 +634fpm +1.6rot 16.5dB -0.7kHz gps2x3\n",
              "ICA3D1139>OGFLR,qAS,MyTownOG:/143013h5300.31N/01032.90E'316/062/A=001959 !W56! id093D1139 -118fpm +0.1rot 4.8dB +2.3kHz gps2x4\n",
              "OGNFF0002>APRS,qAS,NAVITER:/193059h4613.98N/01421.83E'000/000/A=000000 !W47! id2AFF0002 +000fpm +0.0rot\n",
              "ICA4B47D2>OGFLR,qAS,LFMF:/193102h4336.89N/00641.98E'000/000/A=000745 !W01! id054B47D2 +000fpm +0.0rot 29.8dB -0.2kHz gps6x7\n",
              -- "FNT051015>OGNFNT,qAS,LSXI1:/193059h4641.18N/00751.53E_084/006g007t060h70 29.5dB -7.7kHz\n",
              "ICA3C6586>OGADSB,qAS,EDFW:/193101h5011.21N/00904.52E^195/245/A=006113 !W68! id253C6586 -1344fpm FL057.70 A3:DLH981 Sq3445\n",
              -- "FNT08A689>OGNFNT,qAS,WHill:/193059h5115.68N\\00005.56Wn !W25! id3F08A689 FNT71 29.8dB -1.3kHz\n",
              "OGNFF0001>APRS,qAS,NAVITER:/193101h4613.98N/01421.83E'000/000/A=000000 !W47! id2AFF0001 +000fpm +0.0rot\n",
              "FLRDF163C>OGFLR,qAS,TuDelft:/193101h5201.92N/00422.77E'134/133/A=000971 !W38! id06DF163C +079fpm -0.6rot 11.5dB -4.0kHz gps1x1\n",
              "FLRDDE1FC>OGFLR,qAS,LECD:/193109h4223.18N/00152.11E'000/000/A=003698 !W74! id06DDE1FC +178fpm +0.0rot 22.8dB +0.5kHz gps12x18\n"
              -- "FNTFC9002>OGNFNT,qAS,LSXI1:/193109h4640.33N/00752.21E_000/002g003t046h98b10311 26.8dB -7.8kHz\n",
              -- "FNT08F968>OGNFNT,qAS,FMGABETZ:/193110h4803.32N/01446.02E_228/007g008t046h66b10284 0.0dB\n"
            ] ::
              [ByteString]
          results = traverse (parse aprsMessageParser "") inputs

      void results `shouldBe` Right ()

    it "parses msg 1" $ do
      let input = "FLRD0095F>OGFLR,qAS,LFNE:/160721h4337.14N/00507.75E'161/061/A=001204 !W82! id06D0095F -157fpm +0.1rot 10.2dB -0.5kHz gps1x2\n" :: ByteString
          result = parse aprsMessageParser "" input

      source <$> result `shouldBe` Right "FLRD0095F"
