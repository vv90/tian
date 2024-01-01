module Common.Validation exposing
    ( Codec
    , CodecError
    , Positive
    , compose
    , fromParser
    , getPositive
    , positive
    , showError
    )

import Parser exposing (Parser, deadEndsToString)


type CodecError
    = CodecError String


type alias Codec i o =
    { decode : i -> Result CodecError o
    , encode : o -> i
    , name : String
    }


showError : CodecError -> String
showError (CodecError msg) =
    msg


compose : Codec b c -> Codec a b -> Codec a c
compose codecBC codecAB =
    Codec
        (codecAB.decode >> Result.andThen codecBC.decode)
        (codecBC.encode >> codecAB.encode)
        (codecAB.name ++ " >> " ++ codecBC.name)


type Positive number
    = Positive number


getPositive : Positive number -> number
getPositive (Positive x) =
    x


positive : Codec number (Positive number)
positive =
    Codec
        (\n ->
            if n > 0 then
                Ok (Positive n)

            else
                Err (CodecError "Must be positive")
        )
        (\(Positive n) -> n)
        "Positive"


fromParser : String -> Parser a -> (a -> String) -> Codec String a
fromParser name parser toString =
    let
        decode =
            Parser.run parser >> Result.mapError (deadEndsToString >> CodecError)
    in
    Codec decode toString name
