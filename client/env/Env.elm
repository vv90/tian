module Env exposing (apiUrl)


apiUrl : String -> String
apiUrl str =
    "/api/" ++ str
