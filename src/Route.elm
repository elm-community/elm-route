module Route ( Router, Route
             , router, match, reverse, route, child
             , prefix, suffix, and, static, custom, string, int
             , (:=), (<//>), (</>), (<>)
             ) where

{-| This module exposes combinators for creating route parsers.

@docs Route, Router

## Routing
@docs route, (:=), router, child, match, reverse

## Route combinators
@docs prefix, (<//>), and, (</>), suffix, (<>), static, custom, string, int
-}

import Combine exposing (Parser)
import Combine.Infix exposing (..)
import Combine.Num
import String


type Component
  = CPrefix String
  | CSuffix String
  | CCustom (String -> Result String ())
  | CString
  | CInt



{-| Routes represent concrete parsers for paths. Routes can be combined
and they keep track of their path components in order to provide
automatic reverse routing.
 -}
type Route a
  = Route { parser : Parser a
          , components : List Component
          }

unroute r =
  case r of
    Route rc ->
      rc

parser     = unroute >> .parser
components = unroute >> .components


{-| A Router is, at its core, a List of Routes.

    sitemap = router [routeA, routeB]

 -}
type Router a
  = Router (Parser a)


unrouter : Router a -> Parser a
unrouter r =
  case r of
    Router p ->
      p


{-| Declare a Route.

    type Sitemap
      = HomeR ()

    homeR : Route Sitemap
    homeR = route HomeR (static "")

 -}
route : (a -> res) -> Route a -> Route res
route f r =
  Route { parser = f <$> parser r
        , components = components r
        }


{-| A synonym for `route`.

    type Sitemap
      = HomeR ()

    homeR : Route Sitemap
    homeR = HomeR := static ""

 -}
(:=) : (a -> res) -> Route a -> Route res
(:=) = route


{-| Construct a Router from a list of Routes.

    type Sitemap
      = HomeR ()
      | BlogR ()

    homeR = HomeR := static ""
    blogR = BlogR := static "blog"
    sitemap = router [homeR, blogR]

 -}
router : List (Route a) -> Router a
router rs =
  List.map (\r -> parser r <* Combine.end) rs
    |> Combine.choice
    |> Router


{-| Prefix a `Route` with a string.

    type Sitemap
      = UserR Int

    userR = UserR := prefix "users" int
    sitemap = router [userR]

    > match sitemap "/users/"
    Nothing : Maybe.Maybe Sitemap

    > match sitemap "/users/1"
    Just (UserR 1) : Maybe.Maybe Sitemap

 -}
prefix : String -> Route res -> Route res
prefix s r =
  Route { parser = Combine.string (s ++ "/") *> parser r
        , components = [CPrefix s] ++ components r
        }


{-| A synonym for `prefix`.

    type Sitemap
      = UserR Int

    userR = UserR := "users" <//> int
    sitemap = router [userR]

    > match sitemap "/users/"
    Nothing : Maybe.Maybe Sitemap

    > match sitemap "/users/1"
    Just (UserR 1) : Maybe.Maybe Sitemap

 -}
(<//>) : String -> Route res -> Route res
(<//>) = prefix


{-| Suffix a `Route` with a string. This can be used in place
of `static` when there are static path components at the end of a
path.

    type Sitemap
      = UserEmailsR Int
      = UserEmailsR' (Int, ())

    userEmailsR = UserEmailsR := suffix "emails" (prefix "users" int)
    userEmailsR' = UserEmailsR' := prefix "users" int `and` static "emails-static"
    sitemap = router [userEmailsR, userEmailsR']

    > match sitemap "/users/1/emails"
    Just (UserEmailsR 1) : Maybe.Maybe Sitemap

    > match sitemap "/users/1/emails-static"
    Just (UserEmailsR' (1,())) : Maybe.Maybe Sitemap

 -}
suffix : String -> Route res -> Route res
suffix s r =
  Route { parser = parser r <* Combine.string ("/" ++ s)
        , components = components r ++ [CSuffix s]
        }


{-| A synonym for `suffix`.

    type Sitemap
      = UserEmailsR Int
      = UserEmailsR' (Int, ())

    userEmailsR = UserEmailsR := "users" <//> int <> "emails"
    userEmailsR' = UserEmailsR' := "users" <//> int </> static "emails"
    sitemap = router [userEmailsR, userEmailsR']

    > match sitemap "/users/1/emails"
    Just (UserEmailsR 1) : Maybe.Maybe Sitemap

    > match sitemap "/users/1/emails-static"
    Just (UserEmailsR' (1,())) : Maybe.Maybe Sitemap

 -}
(<>) : Route res -> String -> Route res
(<>) = flip suffix


{-| Compose two Routes.

    type Sitemap
      = AddR (Int, Int)

    addR = AddR := int `and` int
    sitemap = router [addR]

    > match sitemap "/1/2"
    Just (AddR (1,2)) : Maybe.Maybe Sitemap

 -}
and : Route a -> Route b -> Route (a, b)
and lr rr =
  Route { parser = (,) <$> (parser lr <* Combine.string "/") <*> parser rr
        , components = components lr ++ components rr
        }


{-| A synonym for `and`.

    type Sitemap
      = AddR (Int, Int)

    addR = AddR := int </> int
    sitemap = router [addR]

    > match sitemap "/1/2"
    Just (AddR (1,2)) : Maybe.Maybe Sitemap

 -}
(</>) : Route a -> Route b -> Route (a, b)
(</>) = and


{-| Create a Route that matches a static String.

    type Sitemap
      = BlogR ()

    blogR = BlogR := static "blog"
    sitemap = router [blogR]

    > match sitemap "/blog"
    Just (BlogR ()) : Maybe.Maybe Sitemap

 -}
static : String -> Route ()
static s =
  Route { parser = () <$ Combine.string s
        , components = [CPrefix s]
        }


{-| Create a Route with a custom Parser.

    import Combine exposing (..)
    import Combine.Infix exposing (..)

    type Category
      = Snippet
      | Post

    type Sitemap
      = CategoryR Category

    categoryR = CategoryR := "categories" <//> custom categoryParser
    sitemap = router [categoryR]

    > match sitemap "/categories/a"
    Nothing : Maybe.Maybe Sitemap

    > match sitemap "/categories/Post"
    Just (CategoryR Post) : Maybe.Maybe Sitemap

    > match sitemap "/categories/Snippet"
    Just (CategoryR Snippet) : Maybe.Maybe Sitemap

See `examples/Custom.elm` for a complete example.

 -}
custom : Parser a -> Route a
custom p =
  let
    validator s =
      case Combine.parse p s of
        (Ok _, _) ->
          Ok ()

        (Err ms, _) ->
          Err (String.join " or " ms)
  in
    Route { parser = p
          , components = [CCustom validator]
          }


{-| A Route that matches any string.

    type Sitemap
      = PostR String

    postR = PostR := "posts" <//> string
    sitemap = router [postR]

    > match sitemap "/posts/"
    Nothing : Maybe.Maybe Sitemap

    > match sitemap "/posts/hello-world/test"
    Nothing : Maybe.Maybe Sitemap

    > match sitemap "/posts/hello-world"
    Just (PostR "hello-world") : Maybe.Maybe Sitemap

 -}
string : Route String
string =
  Route { parser = Combine.regex "[^/]+"
        , components = [CString]
        }


{-| A Route that matches any integer.

    type Sitemap
      = UserR Int

    userR = UserR := "users" <//> int
    sitemap = router [userR]

    > match sitemap "/users/a"
    Nothing : Maybe.Maybe Sitemap

    > match sitemap "/users/1"
    Just (UserR 1) : Maybe.Maybe Sitemap

    > match sitemap "/users/-1"
    Just (UserR -1) : Maybe.Maybe Sitemap

 -}
int : Route Int
int =
  Route { parser = Combine.Num.int
        , components = [CInt]
        }


{-| Routers may be nested. This function is useful in situations
where you want to split your routes into multiple types while still
maintaining a single top-level "site map".

    type AdminSitemap
      = AdminHomeR ()
      | AdminUsersR ()

    adminHomeR = AdminHomeR := static "admin"
    adminUsersR = AdminHomeR := "admin" <//> static "users"
    adminSitemap = router [adminHomeR, adminUsersR]

    type Sitemap
      = HomeR ()
      | BlogR ()
      | AdminR AdminSitemap

    homeR = HomeR := static ""
    blogR = BlogR := static "blog"
    sitemap = router [homeR, blogR, child AdminR adminSitemap]

See `examples/Reuse.elm` for a more advanced use case of this.

 -}
child : (a -> b) -> Router a -> Route b
child f r =
  Route { parser = f <$> unrouter r
        , components = []
        }



{-| Given a Router and an arbitrary String representing a path, this
function will return the first Route that matches that path.

    type Sitemap
      = HomeR ()
      | UsersR ()
      | UserR Int

    homeR = HomeR := static ""
    usersR = UsersR := static "users"
    usersR = UserR := "users" <//> int
    sitemap = router [homeR, userR, usersR]

    > match siteMap "/a"
    Nothing : Maybe.Maybe Sitemap

    > match siteMap "/"
    Just (HomeR ()) : Maybe.Maybe Sitemap

    > match siteMap "/users"
    Just (UsersR ()) : Maybe.Maybe Sitemap

    > match siteMap "/users/1"
    Just (UserR 1) : Maybe.Maybe Sitemap

    > match siteMap "/users/1"
    Just (UserR 1) : Maybe.Maybe Sitemap

 -}
match : Router a -> String -> Maybe a
match r path =
  case String.uncons path of
    Just ('/', path) ->
      Combine.parse (unrouter r) path
        |> Result.toMaybe << fst

    _ ->
      Nothing


{-| Render a path given a route and a list of route components.

    type Sitemap
      = HomeR ()
      | UsersR ()
      | UserR Int

    homeR = HomeR := static ""
    usersR = UsersR := static "users"
    usersR = UserR := "users" <//> int
    sitemap = router [homeR, userR, usersR]

    > reverse homeR []
    "/"

    > reverse usersR []
    "/users"

    > reverse userR ["1"]
    "/users/1"

If you are willing to write some boilerplate, this function can be used
to construct a reasonably-safe reverse routing function specific to your
application:

    render : Sitemap -> String
    render r =
      case r of
        HomeR () -> reverse homeR []
        UsersR () -> reverse usersR []
        UserR uid -> reverse userR [toString uid]

    > render (HomeR ())
    "/"

    > render (UsersR ())
    "/users"

    > render (UserR 1)
    "/users/1"

This function will crash at runtime if there is a mismatch between the
route and the list of arguments that is passed in. For example:

    > reverse deepR []
    Error: Ran into a `Debug.crash` in module `Route`

    This was caused by the `case` expression between lines 145 and 175.
    One of the branches ended with a crash and the following value got through:

        ([],[CInt,CInt,CInt])

    The message provided by the code author is:

        'reverse' called with an unexpected number of arguments

    > reverse deepR ["a"]
    Error: Ran into a `Debug.crash` in module `Route`

    This was caused by the `case` expression between lines 171 and 176.
    One of the branches ended with a crash and the following value got through:

        Err ("could not convert string 'a' to an Int")

    The message provided by the code author is:

        could not convert string 'a' to an Int in a call to 'reverse'

 -}
reverse : Route a -> List String -> String
reverse r inputs =
  let
    accumulate cs is xs =
      case (is, xs) of
        ([], []) ->
          "/" ++ (String.join "/" (List.reverse cs))

        (_, CPrefix p :: xs) ->
          accumulate (p :: cs) is xs

        (_, CSuffix p :: xs) ->
          accumulate (p :: cs) is xs

        (i :: is, CCustom p :: xs) ->
          case p i of
            Ok _ ->
              accumulate (i :: cs) is xs

            Err m ->
              Debug.crash (m ++ " in a call to 'reverse' but received '" ++ i ++ "'")

        (i :: is, CString :: xs) ->
          accumulate (i :: cs) is xs

        (i :: is, CInt :: xs) ->
          case String.toInt i of
            Ok _ ->
              accumulate (i :: cs) is xs

            Err m ->
              Debug.crash m ++ " in a call to 'reverse'"

        _ ->
          Debug.crash "'reverse' called with an unexpected number of arguments"
  in
    accumulate [] inputs (components r)


infixl 7 :=
infixr 9 <//>
infixl 8 </>
infixr 9 <>
