module Update ( Action(..), Page(..), Model, init, update )  where

import History
import Effects exposing (Effects, none)
import Task

import Data
import Routes exposing (Sitemap(..))

type Page
  = Home
  | Posts (List Data.Post)
  | Post Data.Post
  | About
  | NotFound

type alias Model
  = { page : Page
    }

type Action
  = NoOp
  | PathChange String
  | UpdatePath Sitemap

routeToPage : Sitemap -> Page
routeToPage r =
  case r of
    HomeR () -> Home
    PostsR () -> Posts Data.posts
    PostR id ->
      Data.lookupPost id
        |> Maybe.map Post
        |> Maybe.withDefault NotFound
    AboutR () -> About

update : Action -> Model -> (Model, Effects Action)
update action model =
  case action of
    NoOp ->
      (model, none)

    PathChange p ->
      let
        page =
          case Routes.match p of
            Nothing -> NotFound
            Just route -> routeToPage route
      in
        ({ page = page }, none)

    UpdatePath r ->
      ( model
      , Routes.route r
          |> History.setPath
          |> Task.toMaybe
          |> Task.map (always NoOp)
          |> Effects.task
      )

init : String -> (Model, Effects Action)
init path =
  ( { page = Home }
  , PathChange path |> Task.succeed |> Effects.task
  )
