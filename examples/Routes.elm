module Routes ( AdminSitemap(..), Sitemap(..), match, route ) where

import Route exposing (..)

type AdminSitemap
  = AdminHomeR ()
  | AdminUsersR ()
  | AdminUserR Int

adminHomeR = AdminHomeR := static "admin"
adminUsersR = AdminUsersR := "admin" <//> static "users"
adminUserR = AdminUserR := "admin" <//> "users" <//> int
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
