module Route ( Router, Route
             , router, match, reverse, route, child
             , prefix, and, static, custom, string, int
             , (<$>), (<//>), (</>)
             ) where

{-| This module represents an experimental approach to route parsing.

@docs Route, Router

## Routing
@docs router, child, match

## Reverse routing
@docs reverse

## Route combinators
@docs route, (<$>), prefix, (<//>), and, (</>), static, custom, string, int
-}

import Combine exposing (Parser)
import Combine.Infix exposing (..)
import Combine.Num
import String


type Component
  = CPrefix String
  | CCustom (String -> Result String ())
  | CString
  | CInt
  | CFail String


{-| Routes are the basic building blocks, they may be composed in
arbitrary ways to form concrete parsers for paths. Routers keep track of
their path components in order to provide automatic reverse routing. -}
type alias Route a
  = { parser : Parser a
    , components : List Component
    }


{-| A Router is comprised of a List of Routes. -}
type Router a
  = Router (Parser a)


{-| Construct a Router from a list of Routes.

    type Sitemap
      = HomeR ()
      | BlogR ()

    homeR = HomeR <$> static ""
    blogR = BlogR <$> static "blog"
    sitemap = router [homeR, blogR]
 -}
router : List (Route a) -> Router a
router rs =
  List.map .parser rs
    |> Combine.choice
    |> Router


unwrap : Router a -> Parser a
unwrap r =
  case r of
    Router p ->
      p


{-| Create a Route that is prefixed by a given string followed by '/'. -}
prefix : String -> Route res -> Route res
prefix s r =
  { parser = Combine.string s *> Combine.string "/" *> r.parser
  , components = [CPrefix s] ++ r.components
  }


{-| Compose two Routes. -}
and : Route a -> Route b -> Route (a, b)
and lr rr =
  { parser = (,) `Combine.map` (lr.parser <* Combine.string "/") <*> rr.parser
  , components = lr.components ++ rr.components
  }


{-| Create a Route that matches a static String. -}
static : String -> Route ()
static s =
  { parser = () <$ Combine.string s
  , components = [CPrefix s]
  }


{-| Create a Route with a custom Parser. -}
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
    { parser = p
    , components = [CCustom validator]
    }


{-| A Route that matches any string. -}
string : Route String
string =
  { parser = Combine.regex "[^/]+"
  , components = [CString]
  }


{-| A Route that matches any integer.

 -}
int : Route Int
int =
  { parser = Combine.Num.int
  , components = [CInt]
  }


{-| A synonym for `and`. -}
(</>) : Route a -> Route b -> Route (a, b)
(</>) = and

infixl 8 </>


{-| A synonym for `prefix`. -}
(<//>) : String -> Route res -> Route res
(<//>) = prefix

infixl 9 <//>


{-| Transform the return value of a Route. -}
route : (a -> res) -> Route a -> Route res
route f r =
  { r | parser = f `Combine.map` (Combine.string "/" *> r.parser <* Combine.end) }


{-| A synonym for `route`. -}
(<$>) : (a -> res) -> Route a -> Route res
(<$>) = route

infixl 7 <$>


{-| Routers may be nested. This function is useful in situations
where you want to split your routes into multiple types while still
maintaining a single top-level "site map".

    type AdminSitemap
      = AdminHomeR ()
      | AdminUsersR ()

    adminHomeR = AdminHomeR <$> static "admin"
    adminUsersR = AdminHomeR <$> "admin" <//> static "users"
    adminSitemap = router [adminHomeR, adminUsersR]

    type Sitemap
      = HomeR ()
      | BlogR ()
      | AdminR AdminSitemap

    homeR = HomeR <$> static ""
    blogR = BlogR <$> static "blog"
    sitemap = router [homeR, blogR, child AdminR adminSitemap]

 -}
child : (a -> b) -> Router a -> Route b
child f r =
  { parser = (Combine.map f (unwrap r))
  , components = [CFail "child routes cannot be reverse-routed"]
  }


{-| Given a Router and an arbitrary String representing a path, this
function will return the first Route that matches that path.

    type Sitemap
      = HomeR ()
      | UsersR ()
      | UserR Int

    homeR = HomeR <$> static ""
    usersR = UsersR <$> static "users"
    usersR = UserR <$> "users" <//> int
    sitemap = router [homeR, userR, usersR]

    > match siteMap "/a"
    Nothing : Maybe.Maybe Sitemap

    > match siteMap "/"
    Just (HomeR ()) : Maybe.Maybe Sitemap

    > match siteMap "/users"
    Just (UsersR ()) : Maybe.Maybe Sitemap

    > match siteMap "/users/1"
    Just (UserR 1) : Maybe.Maybe Sitemap

 -}
match : Router a -> String -> Maybe a
match r path =
  Combine.parse (unwrap r) path
    |> Result.toMaybe << fst


{-| Render a path given a route and a list of route components.

    > homeR = static ""
    > reverse homeR []
    "/"

    > deepR = "deep" <//> int </> int </> int
    > reverse deepR ["1", "2", "3"]
    "/deep/1/2/3"

If you are willing to write some boilerplate, this function can be used
to construct a reasonably-safe reverse routing function specific to your
application:

    type Sitemap
      = HomeR ()
      | UsersR ()
      | UserR Int

    homeR = HomeR <$> static ""
    usersR = UsersR <$> static "users"
    usersR = UserR <$> "users" <//> int
    sitemap = router [homeR, userR, usersR]

    render : Sitemap -> String
    render r =
      case r of
        HomeR () -> reverse homeR []
        UsersR () -> reverse usersR []
        UserR uid -> reverse userR [toString uid]

This function will crash at runtime if there is a mismatch between the
route and the list of arguments that is passed in. For example:

    > reverse deepR []
    Error: Ran into a `Debug.crash` in module `Route`

    This was caused by the `case` expression between lines 145 and 175.
    One of the branches ended with a crash and the following value got through:

        ([],[CInt,CInt,CInt])

    The message provided by the code author is:

        invalid arity in a call to 'reverse'

    > reverse deepR ["a"]
    Error: Ran into a `Debug.crash` in module `Route`

    This was caused by the `case` expression between lines 171 and 176.
    One of the branches ended with a crash and the following value got through:

        Err ("could not convert string 'a' to an Int")

    The message provided by the code author is:

        could not convert string 'a' to an Int

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
              Debug.crash m

        (i :: is, CFail m :: xs) ->
          Debug.crash m

        _ ->
          Debug.crash "invalid arity in a call to 'reverse'"
  in
    accumulate [] inputs r.components
