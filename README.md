# elm-route [![Build Status](https://travis-ci.org/elm-community/elm-route.svg)](https://travis-ci.org/elm-community/elm-route)

``` shell
elm package install elm-community/elm-route
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
  | NotFound

home = Home := static ""
about = About := static "about"
users = Users := static "users"
user = User := static "users" </> int
userPosts = UserPosts := static "users" </> int </> static "posts"
userPost = UserPost := static "users" </> int </> string
routes = router [home, about, users, user, userPosts, userPost]

match : String -> Maybe Route
match = Route.match routes

toString : Route -> String
toString route =
  case route of
    Home -> reverse home []
    About -> reverse about []
    Users -> reverse users []
    User id -> reverse user [toString id]
    UserPosts id -> reverse userPosts [toString id]
    UserPost uid pid -> reverse userPost [toString uid, pid]
    NotFound -> Debug.crash "cannot route to NotFound"
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
> import App.Routes as Routes

> Routes.toString Home
"/" : String

> Routes.toString About
"/about" : String

> Routes.toString (UserPost 1 "hello")
"/users/1/hello" : String
```

To use it with [Navigation][nav], define `match` in terms of `Location`

``` elm
match : Location -> Route
match location =
  location.pathname
    |> Routes.match routes
    |> Maybe.withDefault NotFound
```

then use it in your `Program`:

``` elm
import App.Routes as Routes
import Navigation exposing (Location)

type Msg
  = ChangeRoute Route

parseRoute : Location -> Msg
parseRoute =
  Routes.match >> ChangeRoute

main : Program Never Model Msg
main =
    Navigation.program parseRoute
        { init = init
        , update = update
        , view = view
        , subscriptions = subscriptions
        }
```

See the `examples` directory and `tests/Tests.elm` for more.


[route]: http://package.elm-lang.org/packages/elm-community/elm-route/latest/Route
[nav]: http://package.elm-lang.org/packages/elm-lang/navigation/latest
