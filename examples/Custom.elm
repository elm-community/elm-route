module Custom exposing (Category(..), Sitemap(..), match, route)

{-| This module demonstrates how you can use custom parsers to parse
paths.  This example parses routes like `/categories/snippet` and
`/categories/post/5` into `CategoryR Snippet` and `CategoryR (Post 5)`,
respectively.
-}

import Combine exposing (..)
import Combine.Num
import Route exposing (..)


type Category
    = Snippet
    | Post Int


category : Parser s Category
category =
    choice
        [ Snippet <$ Combine.string "snippet"
        , Post <$> (Combine.string "post/" *> Combine.Num.int)
        ]


show : Category -> String
show c =
    case c of
        Snippet ->
            "snippet"

        Post id ->
            "post/" ++ Basics.toString id


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
