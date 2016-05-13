module Main exposing (main)

import Html.App as Html
import Ports exposing (path)
import Update exposing (Flags, Msg(..))
import View


main : Program Flags
main =
    Html.programWithFlags
        { init = Update.init
        , update = Update.update
        , view = View.view
        , subscriptions =
            \_ ->
                Sub.batch [ path PathChanged ]
        }
