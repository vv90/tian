module Common.FormField exposing
    ( FormField
    , getRaw
    , getVal
    , initFormFieldRaw
    , updateFormField
    )

import Common.Validation exposing (Codec, CodecError)


type FormField r v
    = FormField
        { raw : r
        , val : Result CodecError v
        , codec : Codec r v
        , dirty : Bool
        }


getVal : FormField r v -> Result CodecError v
getVal (FormField formField) =
    formField.val


getRaw : FormField r v -> r
getRaw (FormField formField) =
    formField.raw


initFormFieldRaw : Codec r v -> r -> FormField r v
initFormFieldRaw codec raw =
    FormField
        { raw = raw
        , val = codec.decode raw
        , codec = codec
        , dirty = False
        }


updateFormField : r -> FormField r v -> FormField r v
updateFormField raw (FormField formField) =
    FormField
        { raw = raw
        , val = formField.codec.decode raw
        , codec = formField.codec
        , dirty = True
        }
