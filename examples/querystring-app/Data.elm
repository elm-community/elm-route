module Data ( Post, posts, lookupPost ) where

type alias Post
  = { id : Int
    , title : String
    , body : String
    }

posts : List Post
posts =
  [ { id = 1, title = "First Post", body = "This is the first post" }
  , { id = 2, title = "Second Post", body = "This is the second post" }
  , { id = 3, title = "Third Post", body = "This is the third post" }
  , { id = 4, title = "Fourth Post", body = "This is the fourth post" }
  , { id = 5, title = "Fifth Post", body = "This is the fifth post" }
  ]

lookupPost : Int -> Maybe Post
lookupPost id =
  List.filter (\p -> p.id == id) posts
    |> List.head
