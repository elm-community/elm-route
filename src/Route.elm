module Route
    exposing
        ( Router
        , Route
        , route
        , (:=)
        , router
        , match
        , reverse
        , static
        , custom
        , string
        , int
        , and
        , (</>)
        )

{-| This module exposes combinators for creating route parsers.

@docs Route, Router

## Routing
@docs route, (:=), router, match, reverse

## Route combinators
@docs static, custom, string, int, and, (</>)
-}

import Combine exposing (..)
import Combine.Num
import String


type Component
    = CStatic String
    | CCustom (String -> Result String ())
    | CString
    | CInt


{-| Routes represent concrete parsers for paths. Routes can be combined
and they keep track of their path components in order to provide
automatic reverse routing.
-}
type Route a
    = Route
        { parser : Parser () a
        , components : List Component
        }


{-| A Router is, at its core, a List of Routes.

    sitemap : Router a
    sitemap = router [routeA, routeB]

-}
type Router a
    = Router (Parser () a)


{-| Declare a Route.

    type Sitemap
      = HomeR

    homeR : Route Sitemap
    homeR = route HomeR (static "")

-}
route : a -> Route (a -> b) -> Route b
route x (Route r) =
    Route
        { parser = r.parser >>= (\k -> Combine.succeed <| k x)
        , components = r.components
        }


{-| A synonym for `route`.

    type Sitemap
      = HomeR

    homeR : Route Sitemap
    homeR = HomeR := static ""

-}
(:=) : a -> Route (a -> b) -> Route b
(:=) =
    route
infixl 7 :=


{-| Construct a Router from a list of Routes.

    type Sitemap
      = HomeR
      | BlogR

    homeR = HomeR := static ""
    blogR = BlogR := static "blog"
    sitemap = router [homeR, blogR]

-}
router : List (Route a) -> Router a
router rs =
    List.map (\(Route r) -> r.parser <* Combine.end) rs
        |> Combine.choice
        |> Router


{-| Create a Route that matches a static String.

    type Sitemap
      = BlogR

    blogR = BlogR := static "blog"
    sitemap = router [blogR]

    > match sitemap "/blog"
    Just BlogR : Maybe Sitemap

-}
static : String -> Route (a -> a)
static s =
    Route
        { parser = identity <$ Combine.string s
        , components = [ CStatic s ]
        }


{-| Create a Route with a custom Parser.

    import Combine exposing (..)
    import Combine.Infix exposing (..)

    type Category
      = Snippet
      | Post

    type Sitemap
      = CategoryR Category

    categoryR = CategoryR := static "categories" </> custom categoryParser
    sitemap = router [categoryR]

    > match sitemap "/categories/a"
    Nothing : Maybe Sitemap

    > match sitemap "/categories/Post"
    Just (CategoryR Post) : Maybe Sitemap

    > match sitemap "/categories/Snippet"
    Just (CategoryR Snippet) : Maybe Sitemap

See `examples/Custom.elm` for a complete example.

-}
custom : Parser () a -> Route ((a -> b) -> b)
custom p =
    let
        validator s =
            case Combine.parse p s of
                Ok _ ->
                    Ok ()

                Err ( _, _, ms ) ->
                    Err (String.join " or " ms)
    in
        Route
            { parser = (|>) <$> p
            , components = [ CCustom validator ]
            }


{-| A Route that matches any string.

    type Sitemap
      = PostR String

    postR = PostR := static "posts" </> string
    sitemap = router [postR]

    > match sitemap "/posts/"
    Nothing : Maybe Sitemap

    > match sitemap "/posts/hello-world/test"
    Nothing : Maybe Sitemap

    > match sitemap "/posts/hello-world"
    Just (PostR "hello-world") : Maybe Sitemap

-}
string : Route ((String -> a) -> a)
string =
    Route
        { parser = (|>) <$> Combine.regex "[^/]+"
        , components = [ CString ]
        }


{-| A Route that matches any integer.

    type Sitemap
      = UserR Int

    userR = UserR := static "users" </> int
    sitemap = router [userR]

    > match sitemap "/users/a"
    Nothing : Maybe Sitemap

    > match sitemap "/users/1"
    Just (UserR 1) : Maybe Sitemap

    > match sitemap "/users/-1"
    Just (UserR -1) : Maybe Sitemap

-}
int : Route ((Int -> a) -> a)
int =
    Route
        { parser = (|>) <$> Combine.Num.int
        , components = [ CInt ]
        }


{-| Compose two Routes.

    type Sitemap
      = AddR Int Int

    addR = AddR := int `and` int
    sitemap = router [addR]

    > match sitemap "/1/2"
    Just (AddR 1 2) : Maybe Sitemap

-}
and : Route (a -> b) -> Route (b -> c) -> Route (a -> c)
and (Route l) (Route r) =
    Route
        { parser = (>>) <$> l.parser <*> (Combine.string "/" *> r.parser)
        , components = l.components ++ r.components
        }


{-| A synonym for `and`.

    type Sitemap
      = AddR Int Int

    addR = AddR := int </> int
    sitemap = router [addR]

    > match sitemap "/1/2"
    Just (AddR 1 2) : Maybe Sitemap

-}
(</>) : Route (a -> b) -> Route (b -> c) -> Route (a -> c)
(</>) =
    and
infixl 8 </>


{-| Given a Router and an arbitrary String representing a path, this
function will return the first Route that matches that path.

    type Sitemap
      = HomeR
      | UsersR
      | UserR Int

    homeR = HomeR := static ""
    usersR = UsersR := static "users"
    usersR = UserR := static "users" </> int
    sitemap = router [homeR, userR, usersR]

    > match sitemap "/a"
    Nothing : Maybe Sitemap

    > match sitemap "/"
    Just HomeR : Maybe Sitemap

    > match sitemap "/users"
    Just UsersR : Maybe Sitemap

    > match sitemap "/users/1"
    Just (UserR 1) : Maybe Sitemap

    > match sitemap "/users/1"
    Just (UserR 1) : Maybe Sitemap

-}
match : Router a -> String -> Maybe a
match (Router r) path =
    case String.uncons path of
        Just ( '/', path ) ->
            Combine.parse r path
                |> Result.toMaybe
                |> Maybe.map (\( _, _, x ) -> x)

        _ ->
            Nothing


{-| Render a path given a Route and a list of route components.

    type Sitemap
      = HomeR
      | UsersR
      | UserR Int

    homeR = HomeR := static ""
    usersR = UsersR := static "users"
    usersR = UserR := static "users" </> int
    sitemap = router [homeR, userR, usersR]

    > reverse homeR []
    "/"

    > reverse usersR []
    "/users"

    > reverse userR ["1"]
    "/users/1"

If you are willing to write some boilerplate, `reverse` can be used to
construct a type safe reverse routing function specific to your
application:

    toString : Sitemap -> String
    toString r =
      case r of
        HomeR  -> reverse homeR []
        UsersR  -> reverse usersR []
        UserR uid -> reverse userR [toString uid]

    > toString HomeR
    "/"

    > toString UsersR
    "/users"

    > toString (UserR 1)
    "/users/1"

`reverse` will crash at runtime if there is a mismatch between the
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
reverse (Route r) inputs =
    let
        accumulate cs is xs =
            case ( is, xs ) of
                ( [], [] ) ->
                    "/" ++ (String.join "/" (List.reverse cs))

                ( _, (CStatic c) :: xs ) ->
                    accumulate (c :: cs) is xs

                ( i :: is, (CCustom p) :: xs ) ->
                    case p i of
                        Ok _ ->
                            accumulate (i :: cs) is xs

                        Err m ->
                            Debug.crash (m ++ " in a call to 'reverse' but received '" ++ i ++ "'")

                ( i :: is, CString :: xs ) ->
                    accumulate (i :: cs) is xs

                ( i :: is, CInt :: xs ) ->
                    case String.toInt i of
                        Ok _ ->
                            accumulate (i :: cs) is xs

                        Err m ->
                            Debug.crash m ++ " in a call to 'reverse'"

                _ ->
                    Debug.crash "'reverse' called with an unexpected number of arguments"
    in
        accumulate [] inputs r.components
