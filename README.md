# elm-route

[![Build Status](https://travis-ci.org/Bogdanp/elm-route.svg)](https://travis-ci.org/Bogdanp/elm-combine)

```shell
elm package install Bogdanp/elm-route
```

A route parsing library for Elm. See the documentation of the `Route`
module for more information. See also [elm-route-parser][erp] for an
alternative approach to route parsing.

## Example

Define your routes:

```
module App.Routes ( AdminSitemap(..), Sitemap(..), match, route ) where

import Route exposing (..)

type AdminSitemap
  = AdminHomeR ()
  | AdminUsersR ()
  | AdminUserR Int

adminHomeR = AdminHomeR := static "admin"
adminUsersR = AdminUsersR := static "admin/users"
adminUserR = AdminUserR := "admin/users" <//> int
adminSitemap = router [adminHomeR, adminUsersR, adminUserR]

routeAdmin : AdminSitemap -> String
routeAdmin r =
  case r of
    AdminHomeR () -> reverse adminHomeR []
    AdminUsersR () -> reverse adminUsersR []
    AdminUserR id -> reverse adminUserR [toString id]

type Sitemap
  = HomeR ()
  | AboutR ()
  | UsersR ()
  | UserR Int
  | UserPostsR Int
  | UserPostR (Int, String)
  | AdminR AdminSitemap

homeR = HomeR := static ""
aboutR = AboutR := static "about"
usersR = UsersR := static "users"
userR = UserR := "users" <//> int
userPostsR = UserPostsR := "users" <//> int <> "posts"
userPostR = UserPostR := "users" <//> int </> string
sitemap = router [homeR, aboutR, usersR, userR, userPostsR, userPostR, child AdminR adminSitemap]

match : String -> Maybe Sitemap
match = Route.match sitemap

route : Sitemap -> String
route r =
  case r of
    HomeR () -> reverse homeR []
    AboutR () -> reverse aboutR []
    UsersR () -> reverse usersR []
    UserR id -> reverse userR [toString id]
    UserPostsR id -> reverse userPostsR [toString id]
    UserPostR (uid, pid) -> reverse userPostR [toString uid, pid]
    AdminR r -> routeAdmin r
```

Then use them:

    > import App.Routes exposing (..)

    > match "/"
    Just (HomeR ()) : Maybe.Maybe App.Routes.Sitemap

    > match "/"
    Just (HomeR ()) : Maybe.Maybe App.Routes.Sitemap

    > match "/users"
    Just (UsersR ()) : Maybe.Maybe App.Routes.Sitemap

    > match "/i-dont-exist"
    Nothing : Maybe.Maybe App.Routes.Sitemap

    > match "/users/1"
    Just (UserR 1) : Maybe.Maybe App.Routes.Sitemap

    > match "/admin/users/1"
    Just (AdminR (AdminUserR 1)) : Maybe.Maybe App.Routes.Sitemap

    > route (HomeR ())
    "/" : String

    > route (AboutR ())
    "/about" : String

    > route (UserPostR (1, "hello"))
    "/users/1/hello" : String

    > route (AdminR (AdminUserR 1))
    "/admin/users/1" : String

See the `examples` directory and `tests/Test.elm` for more.

[erp]: https://github.com/etaque/elm-route-parser
