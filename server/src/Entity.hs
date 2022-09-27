{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

module Entity where
import Relude
import NavPoint (NavPoint(..))
import Generics.SOP qualified as SOP
import Data.Aeson qualified as Aeson
import Language.Haskell.To.Elm
    ( HasElmType (elmDefinition, elmType)
    , HasElmEncoder (elmEncoderDefinition, elmEncoder)
    , HasElmDecoder (elmDecoderDefinition, elmDecoder)
    , deriveElmTypeDefinition
    , defaultOptions
    , deriveElmJSONDecoder
    , deriveElmJSONEncoder
    )
import Magic.ElmDeriving (ElmType)
import qualified Language.Elm.Type as Type
import qualified Language.Elm.Expression as Expression

data Entity k a = 
    Entity 
        { key :: k
        , entity :: a 
        } 
    deriving (Show, Read, Eq, Generic, SOP.Generic, SOP.HasDatatypeInfo, Aeson.ToJSON, Aeson.FromJSON)
    
instance HasElmType Entity where
    elmDefinition =
        Just $ deriveElmTypeDefinition @Entity defaultOptions "Api.Entity.Entity"

instance HasElmDecoder Aeson.Value Entity where
    elmDecoderDefinition =
        Just $ deriveElmJSONDecoder @Entity defaultOptions Aeson.defaultOptions "Api.Entity.entityDecoder"

instance HasElmEncoder Aeson.Value Entity where
    elmEncoderDefinition =
        Just $ deriveElmJSONEncoder @Entity defaultOptions Aeson.defaultOptions "Api.Entity.entityEncoder"

instance (HasElmType k, HasElmType a) => HasElmType (Entity k a) where
  elmType =
    Type.apps (elmType @Entity) [elmType @k, elmType @a]

instance (HasElmDecoder Aeson.Value k, HasElmDecoder Aeson.Value a) => HasElmDecoder Aeson.Value (Entity k a) where
  elmDecoder =
    Expression.apps (elmDecoder @Aeson.Value @Entity) [elmDecoder @Aeson.Value @k, elmDecoder @Aeson.Value @a]

instance (HasElmEncoder Aeson.Value k, HasElmDecoder Aeson.Value a) => HasElmEncoder Aeson.Value (Entity k a) where
  elmEncoder =
    Expression.apps (elmEncoder @Aeson.Value @Entity) [elmEncoder @Aeson.Value @k, elmDecoder @Aeson.Value @a]