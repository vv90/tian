module Utils.Deferred exposing (..)


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


type AsyncOperationStatus a
    = Started
    | Finished a
