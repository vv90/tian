module Common.ChildUpdate exposing (..)


childUpdate :
    (m -> m1 -> m1)
    -> (msg -> msg1)
    -> (msg -> m -> ( m, Cmd msg ))
    -> msg
    -> m
    -> m1
    -> ( m1, Cmd msg1 )
childUpdate withChildModel withChildCmd upd childMsg childModel model =
    let
        ( newChildModel, childCmd ) =
            upd childMsg childModel
    in
    ( model |> withChildModel newChildModel
    , Cmd.map withChildCmd childCmd
    )


childUpdateWithEffect :
    (m -> m1 -> m1)
    -> (ef -> ( m1 -> m1, Cmd msg1, ef1 ))
    -> (msg -> msg1)
    -> (msg -> m -> ( m, Cmd msg, ef ))
    -> msg
    -> m
    -> m1
    -> ( m1, Cmd msg1, Maybe ef1 )
childUpdateWithEffect withChildModel handleEffect withChildCmd upd childMsg childModel model =
    let
        ( newChildModel, childCmd, childEffect ) =
            upd childMsg childModel

        ( withEffectApplied, effectCmd, newEffect ) =
            handleEffect childEffect
    in
    ( model |> withChildModel newChildModel |> withEffectApplied
    , Cmd.batch
        [ Cmd.map withChildCmd childCmd
        , effectCmd
        ]
    , Just newEffect
    )
