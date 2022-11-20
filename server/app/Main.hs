module Main where

import Relude
import Lib ( startApp )
import Control.Concurrent.Async (concurrently, concurrently_)
import Control.Concurrent (threadDelay)

startCounter :: Int -> IO ()
startCounter n = do
    print n
    threadDelay 1000000
    startCounter (n + 1)

-- runMain :: IO ()
-- runMain =
--     concurrently startApp

main :: IO ()
main = concurrently_ (startApp 8081) (startCounter 1) --startApp
