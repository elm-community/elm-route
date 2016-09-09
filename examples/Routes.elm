module Routes exposing (AdminSitemap(..), Sitemap(..), match, route)

{-| This module demonstates basic usage of the API.

    > match "/"
    Just (HomeR ) : Maybe Routes.Sitemap

    > match "/users"
    Just (UsersR ) : Maybe Routes.Sitemap

    > match "/admin/users/1"
    Just (Admin (AdminUserR 1)) : Maybe Routes.Sitemap

    > match "/foo"
    Nothing : Maybe Routes.Sitemap
-}

import Route exposing (..)


type AdminSitemap
    = AdminHomeR
    | AdminUsersR
    | AdminUserR Int


adminHomeR =
    AdminHomeR := static "admin"


adminUsersR =
    AdminUsersR := static "admin" </> static "users"


adminUserR =
    AdminUserR := static "admin" </> static "users" </> int


adminSitemap =
    router [ adminHomeR, adminUsersR, adminUserR ]


routeAdmin : AdminSitemap -> String
routeAdmin r =
    case r of
        AdminHomeR ->
            reverse adminHomeR []

        AdminUsersR ->
            reverse adminUsersR []

        AdminUserR id ->
            reverse adminUserR [ toString id ]


type Sitemap
    = HomeR
    | AboutR
    | UsersR
    | UserR Int
    | UserPostsR Int
    | UserPostR Int String
    | AdminR AdminSitemap


homeR =
    HomeR := static ""


aboutR =
    AboutR := static "about"


usersR =
    UsersR := static "users"


userR =
    UserR := static "users" </> int


userPostsR =
    UserPostsR := static "users" </> int </> static "posts"


userPostR =
    UserPostR := static "users" </> int </> string


sitemap =
    router [ homeR, aboutR, usersR, userR, userPostsR, userPostR, map AdminR adminSitemap ]


match : String -> Maybe Sitemap
match =
    Route.match sitemap


route : Sitemap -> String
route r =
    case r of
        HomeR ->
            reverse homeR []

        AboutR ->
            reverse aboutR []

        UsersR ->
            reverse usersR []

        UserR id ->
            reverse userR [ toString id ]

        UserPostsR id ->
            reverse userPostsR [ toString id ]

        UserPostR uid pid ->
            reverse userPostR [ toString uid, pid ]

        AdminR r ->
            routeAdmin r
