module Tests where

import ElmTest exposing (..)

import Example exposing (..)
import Route exposing (match, reverse)
import String


matching : Test
matching =
  suite "Matching"
    [ test "Match home" (assertEqual (Just (Home ())) (match siteMap "/"))
    , test "Match users" (assertEqual (Just (Users ())) (match siteMap "/users"))
    , test "Match user" (assertEqual (Just (User 1)) (match siteMap "/users/1"))
    , test "Fail user" (assertEqual Nothing (match siteMap "/users/a"))
    , test "Match user emails"
        <| assertEqual
             (Just (UserEmails (1, ())))
             (match siteMap "/users/1/emails")
    , test "Match user email"
        <| assertEqual
             (Just (UserEmail (1, 1)))
             (match siteMap "/users/1/emails/1")
    , test "Match deep"
        <| assertEqual
             (Just (Deep ((1, 2), 3)))
             (match siteMap "/deep/1/2/3")
    , test "Match custom Foo" (assertEqual (Just (Custom' Foo)) (match siteMap "/custom/Foo"))
    , test "Match custom Bar" (assertEqual (Just (Custom' Bar)) (match siteMap "/custom/Bar"))
    , test "Fail custom" (assertEqual Nothing (match siteMap "/custom/Baz"))
    , test "Match admin" (assertEqual (Just (Admin (AdminHome ()))) (match siteMap "/admin"))
    , test "Not found" (assertEqual Nothing (match siteMap "/i-dont-exist"))
    ]


reversing : Test
reversing =
  suite "Reversing"
    [ test "Reverse home" (assertEqual "/" (reverse homeR []))
    , test "Reverse user" (assertEqual "/user/1" (reverse userR ["1"]))
    , test "Reverse custom" (assertEqual "/custom/Foo" (reverse customR ["Foo"]))
    ]


all : Test
all =
  suite "Tests" [ matching ]
