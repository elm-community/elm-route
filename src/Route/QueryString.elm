module Route.QueryString ( QueryString
                         , parse, empty
                         , render, add, remove, filter
                         , all, one, many
                         , string, int
                         ) where

{-| This module exposes functions for working with query strings.

You can manipulate query strings:

    empty
      |> add "a" "hello"
      |> add "a" "goodbye"
      |> add "b" "1"
      |> render

And you can parse and extract their parameters:

    let
      qs = parse "?a=1&a=2&a=test&b=hello"
      a = many int qs
      b = one string qs |> Maybe.withDefault "goodbye"
    in
      (a, b)

## Types
@docs QueryString

## Constructing QueryStrings
@docs parse, empty

## Manipulating parameters
@docs render, add, remove, filter

## Extracting parameters
@docs all, one, many

### Parsers
@docs string, int
-}

import Combine exposing (Parser, end, maybe, or, regex, sepBy, skip)
import Combine.Num
import Combine.Infix exposing ((<$>), (<*>), (<*), (*>))
import Dict exposing (Dict)
import Http exposing (uriDecode, uriEncode)
import String


{-| Represents a parsed query string. -}
type QueryString
  = QueryString (Dict String (List String))


unwrap : QueryString -> (Dict String (List String))
unwrap q =
  case q of
    QueryString d ->
      d


{-| Construct an empty QueryString. -}
empty : QueryString
empty =
  QueryString Dict.empty


{-| Turn a String into a QueryString. The initial `?` is optional.

    > parse ""
    QueryString (Dict.fromList []) : Route.QueryString.QueryString

    > parse "?a=1&b=c&a=2"
    QueryString (Dict.fromList [("a",["1","2"]),("b",["c"])])
        : Route.QueryString.QueryString

    > parse "a=1&b=c&a=2"
    QueryString (Dict.fromList [("a",["1","2"]),("b",["c"])])
        : Route.QueryString.QueryString

 -}
parse : String -> QueryString
parse s =
  case Combine.parse query s of
    (Ok d, _) ->
      QueryString d

    _ ->
      empty


{-| Retrieve all of the values for a given key.

    > parse "?a=1&a=2" |> all "a"
    ["1","2"] : List String

    > parse "?a=1&a=2" |> all "b"
    [] : List String

 -}
all : String -> QueryString -> List String
all k qs =
  unwrap qs
    |> Dict.get k
    |> Maybe.withDefault []


{-| Retrieve a single value for a given key. Values are funneled through
the given parser before being returned.

    > parse "?a=1&a=2" |> one string "a"
    Just "2" : Maybe.Maybe String

    > parse "?a=1&a=2" |> one int "a"
    Just 2 : Maybe.Maybe Int

    > parse "?a=1&a=c" |> one int "a"
    Just 1 : Maybe.Maybe Int

 -}
one : Parser a -> String -> QueryString -> Maybe a
one p k qs =
  many p k qs
    |> List.head


{-| Retrieve zero or more values for some key. Values are funneled
through the given parser before being returned.

    > parse "?a=1&a=c&a=2" |> many int "a"
    [1,2] : List Int

 -}
many : Parser a -> String -> QueryString -> List a
many p k qs =
  all k qs
    |> List.filterMap (maybeParse p)


{-| A Parser that accepts any string. -}
string : Parser String
string =
  regex ".*"


{-| A Parser that accepts any integer. -}
int : Parser Int
int =
  Combine.Num.int


{-| Render a QueryString to a String.

    > parse "?a=1&b=a&a=c" |> render
    "?a=1&a=c&b=a" : String

 -}
render : QueryString -> String
render qs =
  let
    flatten (k, xs) =
      List.map (\x -> k ++ "=" ++ uriEncode x) xs
  in
    unwrap qs
      |> Dict.toList
      |> List.concatMap flatten
      |> String.join "&"
      |> (++) "?"


{-| Add a value to a key.

    > parse "?a=1&b=a&a=c" |> add "a" "2" |> render
    "?a=2&a=1&a=c&b=a" : String

    > parse "?a=1&b=a&a=c" |> add "d" "hello" |> render
    "?a=1&a=c&b=a&d=hello" : String

 -}
add : String -> String -> QueryString -> QueryString
add k v qs =
  let
    prepend xs =
      case xs of
        Nothing -> Just [v]
        Just xs -> Just (v::xs)
  in
    unwrap qs
      |> Dict.update k prepend
      |> QueryString


{-| Remove a key.

    > parse "?a=1&b=a&a=c" |> remove "a" |> render
    "?b=a" : String

    > parse "?a=1&b=a&a=c" |> remove "c" |> render
    "?a=1&a=c&b=a" : String

 -}
remove : String -> QueryString -> QueryString
remove k qs =
  unwrap qs
    |> Dict.remove k
    |> QueryString


{-| Filter a key's values.

    > parse "?a=1&b=a&a=c" |> filter "a" ((==) "1") |> render
    "?a=1&b=a" : String

 -}
filter : String -> (String -> Bool) -> QueryString -> QueryString
filter k f qs =
  let
    remove xs =
      Maybe.map (List.filter f) xs
  in
    unwrap qs
      |> Dict.update k remove
      |> QueryString


parameter : Parser (String, String)
parameter =
  let
    key = regex "[^=]+"
    value = regex "[^&]*"

    param k v =
      (k, uriDecode v)
  in
    param <$> key <* Combine.string "=" <*> value


parameters : Parser (List (String, String))
parameters =
  sepBy (Combine.string "&") parameter <* (skip (Combine.string "#") `or` end)


query : Parser (Dict String (List String))
query =
  let
    prepend y xs =
      case xs of
        Nothing -> Just [y]
        Just xs -> Just (y :: xs)

    collect (k, x) d =
      Dict.update k (prepend x) d
  in
    List.foldr collect Dict.empty <$> (maybe (Combine.string "?") *> parameters)


maybeParse : Parser a -> String -> Maybe a
maybeParse p =
  Result.toMaybe << fst << Combine.parse p
