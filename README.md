# elm-route [![Build Status](https://travis-ci.org/Bogdanp/elm-route.svg)](https://travis-ci.org/Bogdanp/elm-route)

``` shell
elm package install Bogdanp/elm-route
```

This library defines functions for constructing route parsers.  See
the documentation of the [Route](src/Route.elm) module for more
information.  A full example is available at `examples/app`.

See also [elm-route-parser][erp] for an alternative approach to route
parsing.

## Usage

First define your routes:

```elm
module App.Routes exposing ( AdminSitemap(..), Sitemap(..), match, route )

import Route exposing (..)

type Sitemap
  = HomeR ()
  | AboutR ()
  | UsersR ()
  | UserR Int
  | UserPostsR Int
  | UserPostR (Int, String)

homeR = HomeR := static ""
aboutR = AboutR := static "about"
usersR = UsersR := static "users"
userR = UserR := "users" <//> int
userPostsR = UserPostsR := "users" <//> int <> "posts"
userPostR = UserPostR := "users" <//> int </> string
sitemap = router [homeR, aboutR, usersR, userR, userPostsR, userPostR]

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
```

You may then use them to match routes:

```elm
> import App.Routes exposing (..)

> match "/"
Just (HomeR ()) : Maybe.Maybe App.Routes.Sitemap

> match "/users"
Just (UsersR ()) : Maybe.Maybe App.Routes.Sitemap

> match "/i-dont-exist"
Nothing : Maybe.Maybe App.Routes.Sitemap

> match "/users/a"
Nothing : Maybe.Maybe App.Routes.Sitemap

> match "/users/1"
Just (UserR 1) : Maybe.Maybe App.Routes.Sitemap

> match "/users/1/hello-world"
Just (UserPostR (1, "hello-world")) : Maybe.Maybe App.Routes.Sitemap
```

Or to render routes:

```elm
> route (HomeR ())
"/" : String

> route (AboutR ())
"/about" : String

> route (UserPostR (1, "hello"))
"/users/1/hello" : String
```

See the `examples` directory and `tests/Tests.elm` for more.


[erp]: https://github.com/etaque/elm-route-parser
