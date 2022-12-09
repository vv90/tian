module Demo.DemoTask where

import Relude
import Entity (Entity(Entity))
import FlightTask (FlightTask)
import Control.Monad.Except (withExceptT)
import Data.Aeson (eitherDecodeFileStrict')

loadDemoTask :: ExceptT String IO (Entity Int32 FlightTask)
loadDemoTask = 
    withExceptT (mappend "Failed to load task: ")
        $ fmap (\x -> Entity 0 x :: Entity Int32 FlightTask) 
        $ ExceptT 
        $ eitherDecodeFileStrict' "./demo/task.json"