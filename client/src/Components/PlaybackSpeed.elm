module Components.PlaybackSpeed exposing (..)


type PlaybackSpeed
    = X1
    | X2
    | X5
    | X10
    | X20
    | X100
    | X500
    | X1000


playbackCoefficient : PlaybackSpeed -> Int
playbackCoefficient speed =
    case speed of
        X1 ->
            1

        X2 ->
            2

        X5 ->
            5

        X10 ->
            10

        X20 ->
            20

        X100 ->
            100

        X500 ->
            500

        X1000 ->
            1000


lowerSpeed : PlaybackSpeed -> Maybe PlaybackSpeed
lowerSpeed speed =
    case speed of
        X1 ->
            Nothing

        X2 ->
            Just X1

        X5 ->
            Just X2

        X10 ->
            Just X5

        X20 ->
            Just X10

        X100 ->
            Just X20

        X500 ->
            Just X100

        X1000 ->
            Just X500


increaseSpeed : PlaybackSpeed -> Maybe PlaybackSpeed
increaseSpeed speed =
    case speed of
        X1 ->
            Just X2

        X2 ->
            Just X5

        X5 ->
            Just X10

        X10 ->
            Just X20

        X20 ->
            Just X100

        X100 ->
            Just X500

        X500 ->
            Just X1000

        X1000 ->
            Nothing
