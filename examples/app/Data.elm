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
  ]

lookupPost : Int -> Maybe Post
lookupPost id =
  List.filter (\p -> p.id == id) posts
    |> List.head
