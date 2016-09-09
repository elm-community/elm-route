module Routes exposing (Sitemap(..), parsePath, navigateTo, toString)

import Navigation exposing (Location)
import Route exposing (..)


type Sitemap
    = HomeR
    | PostsR
    | PostR Int
    | AboutR
    | NotFoundR


homeR =
    HomeR := static ""


postsR =
    PostsR := static "posts"


postR =
    PostR := static "posts" </> int


aboutR =
    AboutR := static "about"


sitemap =
    router [ homeR, postsR, postR, aboutR ]


match : String -> Sitemap
match =
    Route.match sitemap
        >> Maybe.withDefault NotFoundR


toString : Sitemap -> String
toString r =
    case r of
        HomeR ->
            reverse homeR []

        PostsR ->
            reverse postsR []

        PostR id ->
            reverse postR [ Basics.toString id ]

        AboutR ->
            reverse aboutR []

        NotFoundR ->
            Debug.crash "cannot render NotFound"


parsePath : Location -> Sitemap
parsePath =
    .pathname >> match


navigateTo : Sitemap -> Cmd msg
navigateTo =
    toString >> Navigation.newUrl
