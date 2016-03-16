module Main (main) where

import History
import Html exposing (Html)
import Effects exposing (Effects, Never)
import StartApp exposing (start)
import Task exposing (Task)

import Update
import View

app : StartApp.App Update.Model
app =
  start { init = Update.init path
        , view = View.view
        , update = Update.update
        , inputs = [ Signal.map2 (\p q -> Update.PathChange <| p ++ q) History.path search ]
        }

main : Signal Html
main =
  app.html

port tasks : Signal (Task Never ())
port tasks = app.tasks

port path : String
port search : Signal String
