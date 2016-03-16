module Routes ( Sitemap(..), match, route, routeQS ) where

import Regex exposing (HowMany(..), regex, split)
import Route exposing (..)
import Route.QueryString as QS exposing (QueryString)

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

match : String -> (Maybe Sitemap, QueryString)
match path =
  case split (AtMost 1) (regex "\\?") path of
    p::qs::_ ->
      (Route.match sitemap p, QS.parse qs)

    [p] ->
      (Route.match sitemap p, QS.empty)

    [] ->
      (Nothing, QS.empty)

route : Sitemap -> String
route r =
  case r of
    HomeR () -> reverse homeR []
    PostsR () -> reverse postsR []
    PostR id -> reverse postR [toString id]
    AboutR () -> reverse aboutR []

routeQS : Sitemap -> QueryString -> String
routeQS r qs =
  route r ++ QS.render qs
