module ReuseTests exposing (all)

import ElmTest exposing (..)
import Route exposing (..)


type Op
    = List ()
    | Read Int
    | Create ()
    | Update Int
    | Delete Int


listR =
    List := static ""


readR =
    Read := int


createR =
    Create := static "new"


updateR =
    Update := int <> "update"


deleteR =
    Delete := int <> "delete"


opRoutes =
    router [ listR, readR, createR, updateR, deleteR ]


renderOp o =
    case o of
        List () ->
            reverse listR []

        Read id ->
            reverse readR [ toString id ]

        Create () ->
            reverse createR []

        Update id ->
            reverse updateR [ toString id ]

        Delete id ->
            reverse deleteR [ toString id ]


type Resource
    = Article Op
    | Book Op
    | User Op


articleR =
    child Article opRoutes


bookR =
    "books" <//> child Book opRoutes


userR =
    "users" <//> child User opRoutes


match =
    Route.match <| router [ articleR, bookR, userR ]


render r =
    case r of
        Article op ->
            renderOp op

        Book op ->
            reverse bookR [] ++ renderOp op

        User op ->
            reverse userR [] ++ renderOp op


matching : Test
matching =
    suite "Matching"
        [ test "Match article list"
            <| assertEqual (Just (Article (List ())))
                (match "/")
        , test "Match book list"
            <| assertEqual (Just (Book (List ())))
                (match "/books/")
        , test "Match user list"
            <| assertEqual (Just (User (List ())))
                (match "/users/")
        , test "Match article create"
            <| assertEqual (Just (Article (Create ())))
                (match "/new")
        , test "Match article edit"
            <| assertEqual (Just (Article (Update 1)))
                (match "/1/update")
        , test "Match user delete"
            <| assertEqual (Just (User (Delete 1)))
                (match "/users/1/delete")
        , test "Fail article match"
            <| assertEqual Nothing
                (match "/a/update")
        , test "Match book read"
            <| assertEqual (Just (Book (Read 1)))
                (match "/books/1")
        , test "Fail book match"
            <| assertEqual Nothing
                (match "/books/foo")
        ]


reversing : Test
reversing =
    suite "Reversing"
        [ test "Reverse article list" (assertEqual "/" (render (Article (List ()))))
        , test "Reverse article delete" (assertEqual "/1/delete" (render (Article (Delete 1))))
        , test "Reverse user update" (assertEqual "/users/1/update" (render (User (Update 1))))
        , test "Reverse book create" (assertEqual "/books/new" (render (Book (Create ()))))
        ]


all : Test
all =
    suite "Reuse" [ matching, reversing ]
