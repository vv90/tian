module Common.Deferred exposing (AsyncOperationStatus(..), Deferred(..), deferredIsPending, deferredToMaybe, setPending)


type Deferred a
    = NotStarted
    | InProgress
    | Updating a
    | Resolved a


setPending : Deferred a -> Deferred a
setPending d =
    case d of
        NotStarted ->
            InProgress

        Resolved x ->
            Updating x

        _ ->
            d


deferredToMaybe : Deferred a -> Maybe a
deferredToMaybe d =
    case d of
        Resolved x ->
            Just x

        Updating x ->
            Just x

        _ ->
            Nothing


deferredIsPending : Deferred a -> Bool
deferredIsPending d =
    case d of
        NotStarted ->
            False

        InProgress ->
            True

        Updating _ ->
            True

        Resolved _ ->
            False


type AsyncOperationStatus a
    = Started
    | Finished a
