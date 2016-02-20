module Route ( Router, Route
             , match, reverse, route, map
             , prefix, and, static, custom, string, int
             , (<$>), (<//>), (</>)
             ) where

{-| This module represents an experimental approach to route parsing.

@docs Route, Router

# Using Routes
@docs reverse

# Using Routers
@docs match

# Route combinators
@docs route, (<$>), prefix, (<//>), and, (</>), static, custom, string, int

# Transforming Routers
@docs map
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


{-| Routes are the basic building block, they may be composed in
aritray ways to form concrete parsers for paths. -}
type alias Route a
  = { parser : Parser a
    , components : List Component
    }


{-| A Router is just a List of Routes. -}
type alias Router a
  = List (Route a)


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


{-| A Route that matches any integer. -}
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
route f rc =
  { rc | parser = f `Combine.map` (Combine.string "/" *> rc.parser <* Combine.end) }


{-| A synonym for `route`. -}
(<$>) : (a -> res) -> Route a -> Route res
(<$>) = route

infixl 7 <$>


{-| Transform the results of a router. -}
map : (a -> b) -> Router a -> Router b
map f routes =
  List.map (\r -> { r | parser = f `Combine.map` r.parser }) routes


{-| Render a path given a route and a list of route parameters.

    > reverse Example.homeR []
    "/"

    > reverse Example.deepR ["1", "2", "3"]
    "/deep/1/2/3"

This function will crash at runtime if there is a mismatch between the
route and the list of arguments that is passed in. For example:

    > reverse Example.deepR []
    Error: Ran into a `Debug.crash` in module `Route`

    This was caused by the `case` expression between lines 145 and 175.
    One of the branches ended with a crash and the following value got through:

        ([],[CInt,CInt,CInt])

    The message provided by the code author is:

        invalid arity in a call to 'reverse'

    > reverse Example.deepR ["a"]
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

        (i :: is, CInt :: xs) ->
          case String.toInt i of
            Ok _ ->
              accumulate (i :: cs) is xs

            Err m ->
              Debug.crash m

        (i :: is, CCustom p :: xs) ->
          case p i of
            Ok _ ->
              accumulate (i :: cs) is xs

            Err m ->
              Debug.crash (m ++ " in a call to 'reverse' but received '" ++ i ++ "'")

        (i :: is, _ :: xs) ->
          accumulate (i :: cs) is xs

        _ ->
          Debug.crash "invalid arity in a call to 'reverse'"
  in
    accumulate [] inputs r.components


{-| Given a list of Routes try to find a match for the given path.

    > match Example.siteMap "/a"
    Nothing : Maybe.Maybe Example.Sitemap

    > match Example.siteMap "/"
    Just (Home ()) : Maybe.Maybe Example.Sitemap

    > match siteMap "/users"
    Just (Users ()) : Maybe.Maybe Example.Sitemap

    > match siteMap "/users/1"
    Just (User 1) : Maybe.Maybe Example.Sitemap

 -}
match : Router a -> String -> Maybe a
match routes path =
  List.map .parser routes
    |> Combine.choice
    |> flip Combine.parse path
    |> Result.toMaybe << fst
