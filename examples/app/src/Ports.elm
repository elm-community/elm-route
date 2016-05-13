port module Ports exposing (path, pushPath)


port path : (String -> msg) -> Sub msg


port pushPath : String -> Cmd msg
