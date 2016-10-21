module Data exposing (Post, fetchPosts, lookupPost)

import Http
import Json.Decode as JD exposing (Decoder, int, string)
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
            JD.map3 Post
                (JD.field "id" int)
                (JD.field "title" string)
                (JD.field "body" string)
    in
        JD.list post


fetchPosts : Task Http.Error (List Post)
fetchPosts =
    Http.get "/api/posts" posts
        |> Http.toTask
