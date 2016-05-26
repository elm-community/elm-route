module Main exposing (main)

import Navigation
import Update exposing (Msg(..), pathParser)
import View


main : Program Never
main =
    Navigation.program (Navigation.makeParser pathParser)
        { init = Update.init
        , update = Update.update
        , urlUpdate = Update.urlUpdate
        , view = View.view
        , subscriptions = \_ -> Sub.batch []
        }
