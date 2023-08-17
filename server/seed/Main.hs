module Main where

import Relude
import Seed.Utils (seed)

main :: IO ()
main = do
  putStrLn "seeding data..."

  runExceptT seed >>= \case
    Left err -> print $ "error: " <> err
    Right _ -> putStrLn "seeding successful"

  pure ()
