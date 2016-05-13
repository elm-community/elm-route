module Reuse exposing (..)

{-| This module demonstrates how you can nest routers.

    > match "/articles/"
    Just (Article (List ())) : Maybe.Maybe Reuse.Resource

    > match "/articles/1"
    Just (Article (Read 1)) : Maybe.Maybe Reuse.Resource

    > match "/books/"
    Just (Book (List ())) : Maybe.Maybe Reuse.Resource

    > match "/books/1"
    Just (Book (Read 1)) : Maybe.Maybe Reuse.Resource
-}

import Route exposing (..)


type Operation
    = List ()
    | Create ()
    | Read Int
    | Update Int
    | Delete Int


listR =
    List := static ""


createR =
    Create := static "create"


readR =
    Read := int


updateR =
    Update := int <> "update"


deleteR =
    Delete := int <> "delete"


operationRouter =
    router [ listR, createR, readR, updateR, deleteR ]


renderOperation : Operation -> String
renderOperation o =
    case o of
        List () ->
            Route.reverse listR []

        Create () ->
            Route.reverse createR []

        Read id ->
            Route.reverse readR [ toString id ]

        Update id ->
            Route.reverse updateR [ toString id ]

        Delete id ->
            Route.reverse deleteR [ toString id ]


type Resource
    = Article Operation
    | Book Operation
    | User Operation


articleR =
    "articles" <//> child Article operationRouter


bookR =
    "books" <//> child Book operationRouter


userR =
    "users" <//> child User operationRouter


resourceRouter =
    router [ articleR, bookR, userR ]


match : String -> Maybe Resource
match =
    Route.match resourceRouter


render : Resource -> String
render r =
    case r of
        Article op ->
            Route.reverse articleR [] ++ renderOperation op

        Book op ->
            Route.reverse bookR [] ++ renderOperation op

        User op ->
            Route.reverse userR [] ++ renderOperation op
