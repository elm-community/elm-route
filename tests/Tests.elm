module Tests exposing (..)

import Combine
import Combine.Infix exposing ((<$))
import Expect
import Fuzz exposing (Fuzzer, tuple, tuple3)
import Route exposing (..)
import String
import Test exposing (..)


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


routes : Router Sitemap
routes =
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


ints1 : Fuzzer Int
ints1 =
    Fuzz.int


ints2 : Fuzzer ( Int, Int )
ints2 =
    tuple ( Fuzz.int, Fuzz.int )


ints3 : Fuzzer ( Int, Int, Int )
ints3 =
    tuple3 ( Fuzz.int, Fuzz.int, Fuzz.int )


matching : Test
matching =
    let
        equal x path _ =
            Expect.equal x (match routes path)

        matches =
            Just >> equal

        matches' x path =
            matches x path ()

        fails =
            equal Nothing
    in
        describe "match"
            [ test
                "fails to match on parse failure"
                (fails "/users/a")
            , test
                "fails to match on custom parse failure"
                (fails "/custom/abc")
            , test
                "fails to match nonexistent paths"
                (fails "/i-dont-exist")
            , test
                "matches the root path"
                (matches Home "/")
            , test
                "matches static paths"
                (matches Users "/users")
            , test
                "matches custom ADT routes"
                (matches (Custom' Foo) "/custom/Foo")
            , test
                "matches custom ADT routes"
                (matches (Custom' Bar) "/custom/Bar")
            , fuzz ints1
                "matches one dynamic segment"
                (\x -> matches' (User x) ("/users/" ++ toString x))
            , fuzz ints1
                "matches one suffixed dynamic segment"
                (\x -> matches' (UserEmails x) ("/users/" ++ toString x ++ "/emails"))
            , fuzz ints2
                "matches two dynamic segments around a static segment"
                (\( x, y ) -> matches' (UserEmail x y) ("/users/" ++ toString x ++ "/emails/" ++ toString y))
            , fuzz ints3
                "matches many dynamic segments"
                (\( x, y, z ) -> matches' (Deep x y z) ("/deep/" ++ String.join "/" (List.map toString [ x, y, z ])))
            ]


reversing : Test
reversing =
    let
        compare parts route params _ =
            Expect.equal
                ("/" ++ String.join "/" parts)
                (reverse route params)
    in
        describe "reverse"
            [ test
                "reverses the root route"
                (compare [] homeR [])
            , test
                "reverses custom routes"
                (\x -> compare [ "custom", "Foo" ] customR [ "Foo" ] ())
            , fuzz ints1
                "reverses dynamic routes"
                (\x -> compare [ "users", toString x ] userR [ toString x ] ())
            ]


rendering : Test
rendering =
    let
        compare parts route _ =
            Expect.equal
                ("/" ++ String.join "/" parts)
                (render route)
    in
        describe "render"
            [ test
                "renders the root route"
                (compare [] Home)
            , test
                "renders static routes"
                (compare [ "users" ] Users)
            , test
                "renders custom parser routes"
                (compare [ "custom", "Foo" ] (Custom' Foo))
            , fuzz ints1
                "renders dynamic routes"
                (\x -> compare [ "users", toString x ] (User x) ())
            , fuzz ints3
                "renders deep dynamic routes"
                (\( x, y, z ) -> compare [ "deep", toString x, toString y, toString z ] (Deep x y z) ())
            ]


all : Test
all =
    describe "elm-route"
        [ matching
        , reversing
        , rendering
        ]
