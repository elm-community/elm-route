# elm-route [![Build Status](https://travis-ci.org/Bogdanp/elm-route.svg)](https://travis-ci.org/Bogdanp/elm-route)

``` shell
elm package install Bogdanp/elm-route
```

This library defines functions for constructing route parsers.  See
the documentation of the [Route][route] module for more information.
A full example is available at `examples/app`.

## Usage

First define your routes:

```elm
module App.Routes exposing ( Route(..), match, route )

import Route exposing (..)

type Route
  = Home
  | About
  | Users
  | User Int
  | UserPosts Int
  | UserPost Int String

home = Home := static ""
about = About := static "about"
users = Users := static "users"
user = User := static "users" </> int
userPosts = UserPosts := static "users" </> int </> static "posts"
userPost = UserPost := static "users" </> int </> string
routes = router [home, about, users, user, userPosts, userPost]

match : String -> Maybe Sitemap
match = Route.match routes

toString : Sitemap -> String
toString route =
  case route of
    Home -> reverse home []
    About -> reverse about []
    Users -> reverse users []
    User id -> reverse user [toString id]
    UserPosts id -> reverse userPosts [toString id]
    UserPost uid pid -> reverse userPost [toString uid, pid]
```

You may then use them to match routes:

```elm
> import App.Routes as Routes exposing (Route(..), match)

> match "/"
Just Home : Maybe Route

> match "/users"
Just Users  : Maybe Route

> match "/i-dont-exist"
Nothing : Maybe Route

> match "/users/a"
Nothing : Maybe Route

> match "/users/1"
Just (User 1) : Maybe Route

> match "/users/1/hello-world"
Just (UserPost 1 "hello-world") : Maybe Route
```

And to convert routes to strings:

```elm
> Routes.toString Home
"/" : String

> Routes.toString About
"/about" : String

> Routes.toString (UserPost 1 "hello")
"/users/1/hello" : String
```

See the `examples` directory and `tests/Tests.elm` for more.


[route]: http://package.elm-lang.org/packages/Bogdanp/elm-route/latest/Route
