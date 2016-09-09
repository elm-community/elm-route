module Tests exposing (..)

import Combine
import Combine.Infix exposing ((<$))
import ElmTest exposing (..)
import Route exposing (..)
import String


type Sitemap
    = Home
    | Users
    | User Int
    | UserEmails Int
    | UserEmail Int Int
    | Deep Int Int Int
    | Custom' Foo


type Foo
    = Foo
    | Bar


fooP =
    Combine.choice
        [ Foo <$ Combine.string "Foo"
        , Bar <$ Combine.string "Bar"
        ]


homeR : Route Sitemap
homeR =
    Home := static ""


usersR : Route Sitemap
usersR =
    Users := static "users"


userR : Route Sitemap
userR =
    User := static "users" </> int


userEmailsR : Route Sitemap
userEmailsR =
    UserEmails := static "users" </> int </> static "emails"


userEmailR : Route Sitemap
userEmailR =
    UserEmail := static "users" </> int </> static "emails" </> int


deepR : Route Sitemap
deepR =
    Deep := static "deep" </> int </> int </> int


customR : Route Sitemap
customR =
    Custom' := static "custom" </> custom fooP


siteMap : Router Sitemap
siteMap =
    router
        [ homeR
        , usersR
        , userR
        , userEmailsR
        , userEmailR
        , deepR
        , customR
        ]


render : Sitemap -> String
render r =
    case r of
        Home ->
            reverse homeR []

        Users ->
            reverse usersR []

        User id ->
            reverse userR [ toString id ]

        UserEmails id ->
            reverse userEmailsR [ toString id ]

        UserEmail uid eid ->
            reverse userEmailR [ toString uid, toString eid ]

        Deep x y z ->
            reverse deepR [ toString x, toString y, toString z ]

        Custom' x ->
            reverse customR [ toString x ]


matching : Test
matching =
    suite "Matching"
        [ test "Match home" <|
            assertEqual (Just Home)
                (match siteMap "/")
        , test "Match users" <|
            assertEqual (Just Users)
                (match siteMap "/users")
        , test "Match user" <|
            assertEqual (Just (User 1))
                (match siteMap "/users/1")
        , test "Fail user" <|
            assertEqual Nothing
                (match siteMap "/users/a")
        , test "Match user emails" <|
            assertEqual (Just (UserEmails 1))
                (match siteMap "/users/1/emails")
        , test "Match user email" <|
            assertEqual (Just (UserEmail 1 1))
                (match siteMap "/users/1/emails/1")
        , test "Match deep" <|
            assertEqual (Just (Deep 1 2 3))
                (match siteMap "/deep/1/2/3")
        , test "Match custom Foo" <|
            assertEqual (Just (Custom' Foo))
                (match siteMap "/custom/Foo")
        , test "Match custom Bar" <|
            assertEqual (Just (Custom' Bar))
                (match siteMap "/custom/Bar")
        , test "Fail custom" <|
            assertEqual Nothing
                (match siteMap "/custom/Baz")
        , test "Not found" <|
            assertEqual Nothing
                (match siteMap "/i-dont-exist")
        ]


reversing : Test
reversing =
    suite "Reversing"
        [ test "Reverse home" (assertEqual "/" (reverse homeR []))
        , test "Reverse user" (assertEqual "/users/1" (reverse userR [ "1" ]))
        , test "Reverse custom" (assertEqual "/custom/Foo" (reverse customR [ "Foo" ]))
        ]


rendering : Test
rendering =
    suite "Reversing Safely"
        [ test "Reverse home" (assertEqual "/" (render (Home)))
        , test "Reverse users" (assertEqual "/users" (render (Users)))
        , test "Reverse user" (assertEqual "/users/1" (render (User 1)))
        , test "Reverse deep" (assertEqual "/deep/1/2/3" (render (Deep 1 2 3)))
        , test "Reverse custom" (assertEqual "/custom/Foo" (render (Custom' Foo)))
        ]


all : Test
all =
    suite "Tests" [ matching, reversing, rendering ]


main =
    runSuite all
