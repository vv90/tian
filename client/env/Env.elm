module Env exposing (..)


apiUrl : String -> String
apiUrl str =
    "http://0.0.0.0:8081/" ++ str
