
module Aprs.Utils where 

import Relude

import Conduit (ConduitT, mapC, takeC, (.|), runConduit, stdoutC)
import Data.Conduit.Network (appSink, appSource, clientSettings, runTCPClient)

authC :: ConduitT ByteString ByteString IO ()
authC =
  mapC (const "user N0CALLX09 pass -1 vers TaskView 0.1 filter r/43.7/5.8/500\r\n")

appC :: ConduitT ByteString Void IO () -> ConduitT ByteString Void IO () -> ConduitT ByteString Void IO ()
appC ptSink responseSink = do
  takeC 1 .| authC .| responseSink
  putStrLn "authenticated"
  ptSink

runAprs :: IO ()
runAprs = runTCPClient (clientSettings 14580 "aprs.glidernet.org") $ \server -> do
  runConduit $ appSource server .| appC stdoutC (appSink server)
