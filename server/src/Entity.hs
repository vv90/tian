module Entity where

import Data.Aeson qualified as Aeson
import Generics.SOP qualified as SOP
import Language.Elm.Definition (Definition)
import Language.Elm.Expression qualified as Expression
import Language.Elm.Type qualified as Type
import Language.Haskell.To.Elm
  ( HasElmDecoder (elmDecoder, elmDecoderDefinition),
    HasElmEncoder (elmEncoder, elmEncoderDefinition),
    HasElmType (elmDefinition, elmType),
    defaultOptions,
    deriveElmJSONDecoder,
    deriveElmJSONEncoder,
    deriveElmTypeDefinition,
  )
import Relude

data Entity k a = Entity
  { key :: k,
    entity :: a
  }
  deriving stock (Show, Read, Eq, Generic)
  deriving anyclass (SOP.Generic, SOP.HasDatatypeInfo, Aeson.ToJSON, Aeson.FromJSON)

instance HasElmType Entity where
  elmDefinition :: Maybe Definition
  elmDefinition =
    Just $ deriveElmTypeDefinition @Entity defaultOptions "Api.Types.Entity"

instance HasElmDecoder Aeson.Value Entity where
  elmDecoderDefinition =
    Just $ deriveElmJSONDecoder @Entity defaultOptions Aeson.defaultOptions "Api.Types.entityDecoder"

instance HasElmEncoder Aeson.Value Entity where
  elmEncoderDefinition =
    Just $ deriveElmJSONEncoder @Entity defaultOptions Aeson.defaultOptions "Api.Types.entityEncoder"

instance (HasElmType k, HasElmType a) => HasElmType (Entity k a) where
  elmType =
    Type.apps (elmType @Entity) [elmType @k, elmType @a]

instance (HasElmDecoder Aeson.Value k, HasElmDecoder Aeson.Value a) => HasElmDecoder Aeson.Value (Entity k a) where
  elmDecoder =
    Expression.apps (elmDecoder @Aeson.Value @Entity) [elmDecoder @Aeson.Value @k, elmDecoder @Aeson.Value @a]

instance (HasElmEncoder Aeson.Value k, HasElmDecoder Aeson.Value a) => HasElmEncoder Aeson.Value (Entity k a) where
  elmEncoder =
    Expression.apps (elmEncoder @Aeson.Value @Entity) [elmEncoder @Aeson.Value @k, elmDecoder @Aeson.Value @a]
