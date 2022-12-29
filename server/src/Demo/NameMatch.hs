module Demo.NameMatch where

import Relude
import Data.Text (Text)
import qualified Data.Aeson as Aeson
import qualified Generics.SOP as SOP
import Language.Haskell.To.Elm (HasElmEncoder, HasElmDecoder, HasElmType)
import Magic.ElmDeriving
import Control.Monad.Except (withExceptT)

data NameMatch = NameMatch 
    { compId :: Text 
    , name :: Text
    }
    deriving (Show, Eq, Ord, Generic, SOP.Generic, SOP.HasDatatypeInfo, Aeson.FromJSON, Aeson.ToJSON)
    deriving (HasElmType, HasElmEncoder Aeson.Value, HasElmDecoder Aeson.Value)
        via ElmType "Api.Demo.NameMatch" NameMatch

loadNames :: ExceptT String IO [NameMatch]
loadNames =
    withExceptT (mappend "Failed to load names: ")
        $ ExceptT 
        $ Aeson.eitherDecodeFileStrict' "./demo/names.json"