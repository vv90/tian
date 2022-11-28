module Env where

import Relude
import Fmt
import System.Environment
import Data.List ((\\))

requiredEnvKeys :: [String]
requiredEnvKeys =
    [ "DB_HOST"
    , "DB_PORT"
    , "DB_USER"
    , "DB_PASS"
    , "DB_NAME"
    ]

checkRequiredEnvironmentKeys :: IO ()
checkRequiredEnvironmentKeys = do
  allEnvKeys <- fmap fst <$> getEnvironment
  let missingKeys = requiredEnvKeys \\ allEnvKeys
  unless (null missingKeys) . error . fmt $
    blockMapF [("Missing env variables" :: Text, blockListF missingKeys)]
