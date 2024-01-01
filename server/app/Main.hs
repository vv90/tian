module Main where

import Backend.Utils (runBackend)
import Env (checkRequiredEnvironmentKeys)
import Relude

-- startCounter :: Int -> IO ()
-- startCounter n = do
--   print n
--   threadDelay 1000000
--   startCounter (n + 1)

main :: IO ()
main = do
  putStrLn "starting server..."
  checkRequiredEnvironmentKeys
  -- print "N52E039"
  -- convertTiffSafe "./demo/ASTGTMV003_N52E039_dem.tif"

  -- print "N51E039"
  -- convertTiffSafe "./demo/ASTGTMV003_N51E039_dem.tif"

  -- convertTiffSafe "./demo/ASTGTMV003_N44E006_dem.tif"
  -- convertTiffSafe "./demo/ASTGTMV003_N45E005_dem.tif"
  runBackend

-- convertTiffSafe "./demo/ASTGTMV003_N45E005_dem.tif"

-- mapM print d
-- pure ()
