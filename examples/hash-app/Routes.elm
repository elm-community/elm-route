module Routes ( Sitemap(..), match, route ) where

import Route exposing (..)
import String

type Sitemap
  = HomeR ()
  | PostsR ()
  | PostR Int
  | AboutR ()

homeR = HomeR := static ""
postsR = PostsR := static "posts"
postR = PostR := "posts" <//> int
aboutR = AboutR := static "about"
sitemap = router [homeR, postsR, postR, aboutR]

match : String -> Maybe Sitemap
match = String.dropLeft 1 >> Route.match sitemap

route : Sitemap -> String
route r =
  let
    route =
      case r of
        HomeR () -> reverse homeR []
        PostsR () -> reverse postsR []
        PostR id -> reverse postR [toString id]
        AboutR () -> reverse aboutR []
  in
    "#" ++ route
