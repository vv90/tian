module Demo.NameMatch where

import Control.Monad.Except (withExceptT)
import Data.Aeson qualified as Aeson
import Generics.SOP qualified as SOP
import Language.Haskell.To.Elm (HasElmDecoder, HasElmEncoder, HasElmType)
import Magic.ElmDeriving
import Relude

data NameMatch = NameMatch
  { compId :: Text,
    name :: Text
  }
  deriving stock (Show, Eq, Ord, Generic)
  deriving anyclass (SOP.Generic, SOP.HasDatatypeInfo, Aeson.FromJSON, Aeson.ToJSON)
  deriving
    (HasElmType, HasElmEncoder Aeson.Value, HasElmDecoder Aeson.Value)
    via ElmType "Api.Demo.NameMatch" NameMatch

loadNames :: ExceptT String IO [NameMatch]
loadNames =
  withExceptT (mappend "Failed to load names: ")
    $ ExceptT
    $ Aeson.eitherDecodeFileStrict' "./demo/names.json"
