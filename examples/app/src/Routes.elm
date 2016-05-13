module Routes exposing (Sitemap(..), match, route)

import Route exposing (..)


type Sitemap
    = HomeR ()
    | PostsR ()
    | PostR Int
    | AboutR ()
    | NotFoundR


homeR =
    HomeR := static ""


postsR =
    PostsR := static "posts"


postR =
    PostR := "posts" <//> int


aboutR =
    AboutR := static "about"


sitemap =
    router [ homeR, postsR, postR, aboutR ]


match : String -> Sitemap
match =
    Route.match sitemap
        >> Maybe.withDefault NotFoundR


route : Sitemap -> String
route r =
    case r of
        HomeR () ->
            reverse homeR []

        PostsR () ->
            reverse postsR []

        PostR id ->
            reverse postR [ toString id ]

        AboutR () ->
            reverse aboutR []

        NotFoundR ->
            Debug.crash "cannot route to NotFound"
