module Common.Effect exposing
    ( EffectSet
    , applyAll
    , applyMapAll
    , effect
    , none
    )


type EffectSet a
    = EffectSet (List a)


none : EffectSet a
none =
    EffectSet []


effect : a -> EffectSet a
effect e =
    EffectSet [ e ]


batch : List (EffectSet a) -> EffectSet a
batch effectSets =
    EffectSet (List.concatMap (\(EffectSet effects) -> effects) effectSets)


applyAll :
    (eff -> ( m -> m, Cmd msg ))
    -> EffectSet eff
    -> ( m, Cmd msg )
    -> ( m, Cmd msg )
applyAll applyEffect (EffectSet effs) ( model, cmd ) =
    let
        applySingle ( fm, c1 ) ( m, c ) =
            ( fm m, Cmd.batch [ c1, c ] )
    in
    effs
        |> List.map applyEffect
        |> List.foldl applySingle ( model, cmd )


applyMapAll :
    (eff -> ( m -> m, Cmd msg, EffectSet eff1 ))
    -> EffectSet eff
    -> ( m, Cmd msg, EffectSet eff1 )
    -> ( m, Cmd msg, EffectSet eff1 )
applyMapAll applyEffect (EffectSet effs) ( model, cmd, efct ) =
    let
        applySingle ( fm, c1, e1 ) ( m, c, e ) =
            ( fm m, Cmd.batch [ c, c1 ], batch [ e, e1 ] )
    in
    effs
        |> List.map (\e -> applyEffect e)
        |> List.foldl applySingle ( model, cmd, efct )
