module Data exposing (Post, fetchPosts, lookupPost)

import Http
import Json.Decode as Json exposing ((:=), Decoder, int, string)
import Task exposing (Task)


type alias Post =
    { id : Int
    , title : String
    , body : String
    }


lookupPost : Int -> List Post -> Maybe Post
lookupPost id posts =
    List.filter (\p -> p.id == id) posts
        |> List.head


posts : Decoder (List Post)
posts =
    let
        post =
            Json.object3 Post
                ("id" := int)
                ("title" := string)
                ("body" := string)
    in
        Json.list post


fetchPosts : Task Http.Error (List Post)
fetchPosts =
    Http.get posts "/api/posts"
