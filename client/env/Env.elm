module Env exposing (..)


apiUrl : String -> String
apiUrl str =
    "/api/" ++ str
