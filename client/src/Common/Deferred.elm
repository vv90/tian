module Common.Deferred exposing (..)


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


type AsyncOperationStatus a
    = Started
    | Finished a
