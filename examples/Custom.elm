module Custom exposing ( .. )

import Combine exposing (..)
import Combine.Infix exposing (..)
import Route exposing (..)

-- Parser
-- ~~~~~~

type Category
  = Snippet
  | Post

category : Parser Category
category = choice [ Snippet <$ Combine.string "snippet"
                  , Post <$ Combine.string "post"
                  ]

show : Category -> String
show c =
  case c of
    Snippet -> "snippet"
    Post -> "post"


-- Router
-- ~~~~~~

type Sitemap
  = CategoryR Category

categoryR = CategoryR := "categories" <//> custom category

sitemap : Router Sitemap
sitemap = router [categoryR]

route : Sitemap -> String
route r =
  case r of
    CategoryR c -> reverse categoryR [show c]
