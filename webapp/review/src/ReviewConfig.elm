module ReviewConfig exposing (config)

{-| Do not rename the ReviewConfig module or the config function, because
`elm-review` will look for these.

To add packages that contain rules, add them to this review project using

    `elm install author/packagename`

when inside the directory containing this file.

-}

import NoDebug.Log
import NoDebug.TodoOrToString
import NoExposingEverything
import NoImportingEverything
import NoMissingSubscriptionsCall
import NoMissingTypeAnnotation
import NoMissingTypeAnnotationInLetIn
import NoPrematureLetComputation
import NoRecursiveUpdate
import NoSimpleLetBody
import NoTestValuesInProductionCode
import NoUnoptimizedRecursion
import NoUnsafeDivision
import NoUnused.CustomTypeConstructorArgs
import NoUnused.CustomTypeConstructors
import NoUnused.Dependencies
import NoUnused.Exports
import NoUnused.Modules
import NoUnused.Parameters
import NoUnused.Patterns
import NoUnused.Variables
import NoUselessSubscriptions
import Review.Rule as Rule exposing (Rule)
import Simplify


config : List Rule
config =
    [ NoDebug.Log.rule
    , NoDebug.TodoOrToString.rule
    , NoUnoptimizedRecursion.rule (NoUnoptimizedRecursion.optOutWithComment "IGNORE TCO")
        |> Rule.ignoreErrorsForDirectories [ "src/Components" ]
    , Simplify.rule Simplify.defaults
    , NoTestValuesInProductionCode.rule
        (NoTestValuesInProductionCode.startsWith "stub_")
    , NoMissingSubscriptionsCall.rule
    , NoRecursiveUpdate.rule
    , NoUselessSubscriptions.rule
    , NoUnsafeDivision.rule
        |> Rule.ignoreErrorsForDirectories [ "src/Common", "src/Domain", "src/Components" ]
    , NoUnused.CustomTypeConstructors.rule []
        |> Rule.ignoreErrorsForDirectories [ "src/Components", "src/Demo" ]
    , NoUnused.CustomTypeConstructorArgs.rule
        |> Rule.ignoreErrorsForDirectories [ "src/Components" ]
    , NoUnused.Dependencies.rule
    , NoUnused.Exports.rule
        |> Rule.ignoreErrorsForDirectories [ "src/Common", "src/Domain", "src/Components" ]
    , NoUnused.Modules.rule
        |> Rule.ignoreErrorsForDirectories [ "src/Common", "src/Components" ]
    , NoUnused.Parameters.rule
        |> Rule.ignoreErrorsForDirectories [ "src/Domain" ]
    , NoUnused.Patterns.rule
        |> Rule.ignoreErrorsForDirectories [ "src/Domain" ]
    , NoUnused.Variables.rule
        |> Rule.ignoreErrorsForDirectories [ "src/Common", "src/Domain", "src/Components", "src/Demo" ]
    , NoImportingEverything.rule [ "Element", "Api.Types" ]
    , NoExposingEverything.rule
        |> Rule.ignoreErrorsForFiles [ "src/Common/Palette.elm" ]
    , NoMissingTypeAnnotation.rule
    , NoMissingTypeAnnotationInLetIn.rule
    , NoPrematureLetComputation.rule
    , NoSimpleLetBody.rule
    ]
        |> List.map (Rule.ignoreErrorsForDirectories [ "src/Api/", ".elm-land/" ])
