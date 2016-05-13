module View exposing (view)

import Html exposing (..)
import Html.Attributes exposing (href)
import Html.Events exposing (onClick)
import Data
import Routes exposing (Sitemap(..))
import Update exposing (Msg(..), Model)


notFound : Html Msg
notFound =
    h1 [] [ text "Page not found" ]


home : Html Msg
home =
    div []
        [ h1 [] [ text "Home Page" ]
        , p [] [ link (Routes.PostR 123) "This post does not exist" ]
        ]


about : Html Msg
about =
    h1 [] [ text "About" ]


loading : Html Msg
loading =
    h1 [] [ text "Loading..." ]


post : Data.Post -> Html Msg
post post =
    div []
        [ h1 [] [ text post.title ]
        , p [] [ text post.body ]
        ]


posts : List Data.Post -> Html Msg
posts posts =
    let
        postLink post =
            li [] [ link (Routes.PostR post.id) post.title ]
    in
        div []
            [ h1 [] [ text "Posts" ]
            , ul [] (List.map postLink posts)
            ]


view : Model -> Html Msg
view model =
    div []
        [ ul []
            [ li [] [ link (HomeR ()) "Home" ]
            , li [] [ link (PostsR ()) "Posts" ]
            , li [] [ link (AboutR ()) "About" ]
            ]
        , case model.route of
            HomeR () ->
                home

            PostsR () ->
                if model.ready then
                    posts model.posts
                else
                    loading

            PostR id ->
                case ( model.ready, model.post ) of
                    ( False, _ ) ->
                        loading

                    ( True, Nothing ) ->
                        notFound

                    ( True, Just p ) ->
                        post p

            AboutR () ->
                about

            NotFoundR ->
                notFound
        ]


link : Sitemap -> String -> Html Msg
link route content =
    a [ href "javascript:;", onClick (RouteTo route) ]
        [ text content ]
