module Main where

import Relude
import Lib ( startApp )
import Control.Concurrent.Async (concurrently, concurrently_)
import Control.Concurrent (threadDelay)
import Data.Conduit.Network (runTCPClient, clientSettings, appSource, appSink, AppData)
import Conduit (($$), stdoutC, runConduit, takeC, mapC)
import Data.Conduit ((.|), ConduitT, yield)

startCounter :: Int -> IO ()
startCounter n = do
    print n
    threadDelay 1000000
    startCounter (n + 1)

-- runMain :: IO ()
-- runMain =
--     concurrently startApp

authC :: ConduitT ByteString ByteString IO ()
authC = 
    mapC (const "user N0CALLX09 pass -1 vers TaskView 0.1 filter r/43.7/5.8/500\r\n") 


appC :: ConduitT ByteString Void IO () -> ConduitT ByteString Void IO () -> ConduitT ByteString Void IO ()
appC ptSink responseSink = do
    takeC 1 .| authC .| responseSink
    print "authenticated"
    ptSink

runAprs = runTCPClient (clientSettings 14580 "aprs.glidernet.org") $ \server -> do
    runConduit $ appSource server .| appC stdoutC (appSink server)

main :: IO ()
main = concurrently_ (startApp 8081) runAprs
