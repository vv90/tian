module Main where

import Relude
import Seed.Utils (seedPointNemo)

main :: IO ()
main = do
  putStrLn "seeding data..."

  runExceptT seedPointNemo >>= \case
    Left err -> print $ "error: " <> err
    Right _ -> putStrLn "seeding successful"

  pure ()
