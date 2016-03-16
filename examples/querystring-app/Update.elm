module Update ( Action(..), Page(..), Model, init, update )  where

import History
import Effects exposing (Effects, none)
import Route.QueryString as QS exposing (QueryString)
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
  | UpdatePathQS Sitemap QueryString

routeToPage : Sitemap -> QueryString -> Page
routeToPage r qs =
  case r of
    HomeR () -> Home
    PostsR () ->
      let limit = QS.one QS.int "limit" qs |> Maybe.withDefault 3 in
      Posts (List.take limit Data.posts)
    PostR id ->
      Data.lookupPost id
        |> Maybe.map Post
        |> Maybe.withDefault NotFound
    AboutR () -> About

pathToPage : String -> Page
pathToPage p =
  case Routes.match p of
    (Nothing, _) -> NotFound
    (Just r, qs) -> routeToPage r qs

setPath : String -> Effects Action
setPath p =
  History.setPath p
    |> Task.toMaybe
    |> Task.map (always NoOp)
    |> Effects.task

update : Action -> Model -> (Model, Effects Action)
update action model =
  case action of
    NoOp ->
      (model, none)

    PathChange p ->
      ({ page = pathToPage p }, none)

    UpdatePath r ->
      ( model
      , setPath <| Routes.route r
      )

    UpdatePathQS r qs ->
      ( model
      , setPath <| Routes.routeQS r qs
      )

init : String -> (Model, Effects Action)
init path =
  ( { page = pathToPage path }, none )
