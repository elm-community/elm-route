module View ( view ) where

import Html exposing (..)
import Html.Attributes exposing (href)
import Html.Events exposing (onClick)

import Data
import Routes exposing (Sitemap(..))
import Update exposing (Action(..), Page(..), Model)

type alias Linker
  = Routes.Sitemap -> String -> Html

notFound : Html
notFound = h1 [] [ text "Page not found" ]

home : Linker -> Html
home link =
  div [] [ h1 [] [ text "Home Page" ]
         , p [] [ link (Routes.PostR 123) "This post does not exist" ]
         ]

about : Html
about = h1 [] [ text "About" ]

post : Data.Post -> Html
post post =
  div [] [ h1 [] [ text post.title ]
         , p [] [ text post.body ]
         ]

posts : Linker -> List Data.Post -> Html
posts link ps =
  let
    postLink post =
      li [] [ link (Routes.PostR post.id) post.title ]
  in
    div [] [ h1 [] [ text "Posts" ]
           , ul [] (List.map postLink ps)
           ]

view : Signal.Address Action -> Model -> Html
view address model =
  let
    link route content =
      a
        [ href "javascript:;", onClick address (UpdatePath route) ]
        [ text content ]
  in
    div
      []
      [ ul [] [ li [] [ link (HomeR ()) "Home" ]
              , li [] [ link (PostsR ()) "Posts" ]
              , li [] [ link (AboutR ()) "About" ]
              ]
      , case model.page of
          Home -> home link
          Posts ps -> posts link ps
          Post p -> post p
          About -> about
          NotFound -> notFound
      ]
