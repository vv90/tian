module Main where

import Relude
import Seed.Utils (seed)

main :: IO ()
main = do
  putStrLn "seeding data..."

  seed
  putTextLn "Complete!"

  pass
