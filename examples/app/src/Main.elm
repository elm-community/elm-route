module Main exposing (main)

import Navigation
import Routes
import Update
import View


main : Program Never
main =
    Navigation.program (Navigation.makeParser Routes.parsePath)
        { init = Update.init
        , update = Update.update
        , urlUpdate = Update.urlUpdate
        , view = View.view
        , subscriptions = \_ -> Sub.batch []
        }
