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
  | CFail String


type alias RouteComponent a
  = { parser : Parser a
    , components : List Component
    }


{-| Routes represent concrete parsers for paths.  Every `Route` is
composed from one or more `RouteComponent`s using the provided
combinators.

Routes also keep track of their path components in order to provide
automatic reverse routing.
 -}
type Route a
  = Route (RouteComponent a)


unroute : Route a -> RouteComponent a
unroute r =
  case r of
    Route c ->
      c


{-| A Router is comprised of a List of Routes. -}
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
route : (a -> res) -> RouteComponent a -> Route res
route f r =
  Route { r | parser = f <$> (Combine.string "/" *> r.parser <* Combine.end) }


{-| A synonym for `route`.

    type Sitemap
      = HomeR ()

    homeR : Route Sitemap
    homeR = HomeR := static ""

 -}
(:=) : (a -> res) -> RouteComponent a -> Route res
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
  List.map (.parser << unroute) rs
    |> Combine.choice
    |> Router


{-| Prefix a `RouteComponent` with a string.

    type Sitemap
      = UserR Int

    userR = UserR := prefix "users" int
    sitemap = router [userR]

    > match sitemap "/users/"
    Nothing : Maybe.Maybe Sitemap

    > match sitemap "/users/1"
    Just (UserR 1) : Maybe.Maybe Sitemap

 -}
prefix : String -> RouteComponent res -> RouteComponent res
prefix s r =
  { parser = Combine.string (s ++ "/") *> r.parser
  , components = [CPrefix s] ++ r.components
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
(<//>) : String -> RouteComponent res -> RouteComponent res
(<//>) = prefix


{-| Suffix a `RouteComponent` with a string. This can be used in place
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
suffix : String -> RouteComponent res -> RouteComponent res
suffix s r =
  { parser = r.parser <* Combine.string ("/" ++ s)
  , components = r.components ++ [CSuffix s]
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
(<>) : RouteComponent res -> String -> RouteComponent res
(<>) = flip suffix


{-| Compose two RouteComponents.

    type Sitemap
      = AddR (Int, Int)

    addR = AddR := int `and` int
    sitemap = router [addR]

    > match sitemap "/1/2"
    Just (AddR (1,2)) : Maybe.Maybe Sitemap

 -}
and : RouteComponent a -> RouteComponent b -> RouteComponent (a, b)
and lr rr =
  { parser = (,) <$> (lr.parser <* Combine.string "/") <*> rr.parser
  , components = lr.components ++ rr.components
  }


{-| A synonym for `and`.

    type Sitemap
      = AddR (Int, Int)

    addR = AddR := int </> int
    sitemap = router [addR]

    > match sitemap "/1/2"
    Just (AddR (1,2)) : Maybe.Maybe Sitemap

 -}
(</>) : RouteComponent a -> RouteComponent b -> RouteComponent (a, b)
(</>) = and


{-| Create a RouteComponent that matches a static String.

    type Sitemap
      = BlogR ()

    blogR = BlogR := static "blog"
    sitemap = router [blogR]

    > match sitemap "/blog"
    Just (BlogR ()) : Maybe.Maybe Sitemap

 -}
static : String -> RouteComponent ()
static s =
  { parser = () <$ Combine.string s
  , components = [CPrefix s]
  }


{-| Create a RouteComponent with a custom Parser.

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
custom : Parser a -> RouteComponent a
custom p =
  let
    validator s =
      case Combine.parse p s of
        (Ok _, _) ->
          Ok ()

        (Err ms, _) ->
          Err (String.join " or " ms)
  in
    { parser = p
    , components = [CCustom validator]
    }


{-| A RouteComponent that matches any string.

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
string : RouteComponent String
string =
  { parser = Combine.regex "[^/]+"
  , components = [CString]
  }


{-| A RouteComponent that matches any integer.

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
int : RouteComponent Int
int =
  { parser = Combine.Num.int
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

 -}
child : (a -> b) -> Router a -> Route b
child f r =
  Route { parser = f <$> unrouter r
        , components = [CFail "nested routers cannot be reverse-routed"]
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
  Combine.parse (unrouter r) path
    |> Result.toMaybe << fst


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

        (i :: is, CFail m :: xs) ->
          Debug.crash m ++ " in a call to 'reverse'"

        _ ->
          Debug.crash "'reverse' called with an unexpected number of arguments"
  in
    accumulate [] inputs (unroute r |> .components)


infixl 7 :=
infixr 9 <//>
infixl 8 </>
infixr 9 <>
