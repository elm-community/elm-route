module Custom exposing (Category(..), Sitemap(..), match, route)

{-| This module demonstrates how you can use custom parsers to parse
paths.  This example parses routes like `/categories/snippet` and
`/categories/post` into `CategoryR Snippet` and `CategoryR Post`,
respectively.
-}

import Combine exposing (..)
import Route exposing (..)


type Category
    = Snippet
    | Post


category : Parser s Category
category =
    choice
        [ Snippet <$ Combine.string "snippet"
        , Post <$ Combine.string "post"
        ]


show : Category -> String
show c =
    case c of
        Snippet ->
            "snippet"

        Post ->
            "post"


type Sitemap
    = CategoryR Category


categoryR =
    CategoryR := static "categories" </> custom category


sitemap : Router Sitemap
sitemap =
    router [ categoryR ]


match : String -> Maybe Sitemap
match =
    Route.match sitemap


route : Sitemap -> String
route r =
    case r of
        CategoryR c ->
            reverse categoryR [ show c ]
