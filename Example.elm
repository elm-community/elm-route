module Example where

import Combine
import Combine.Infix exposing ((<$))
import Route exposing (..)

type Sitemap
  = Home ()
  | Users ()
  | User Int
  | UserEmails (Int, ())
  | UserEmail (Int, Int)
  | Admin AdminArea
  | Deep ((Int, Int), Int)
  | Custom' Foo

type AdminArea
  = AdminHome ()
  | AdminUsers ()

type Foo
  = Foo
  | Bar

fooP = Combine.choice [ Foo <$ Combine.string "Foo"
                      , Bar <$ Combine.string "Bar"
                      ]

adminHomeR = route AdminHome (static "admin")
adminUsersR = route AdminUsers ("admin" <//> static "users")

adminRoutes : Router AdminArea
adminRoutes = [adminHomeR, adminUsersR]

homeR = Home <$> static ""
usersR = Users <$> static "users"
userR = User <$> "users" <//> int
userEmailsR = UserEmails <$> "users" <//> int </> static "emails"
userEmailR = UserEmail <$> "users" <//> int </> "emails" <//> int
deepR = Deep <$> "deep" <//> int </> int </> int
customR = Custom' <$> "custom" <//> custom fooP

render : Sitemap -> String
render r =
  case r of
    Home _ -> reverse homeR []
    Users _ -> reverse usersR []
    User id -> reverse userR [toString id]
    UserEmails (id, _) -> reverse userEmailsR [toString id]
    UserEmail (uid, eid) -> reverse userEmailR [toString uid, toString eid]
    Deep ((x, y), z) -> reverse deepR [toString x, toString y, toString z]
    Custom' x -> reverse customR [toString x]
    Admin r -> renderAdmin r

renderAdmin : AdminArea -> String
renderAdmin r =
  case r of
    AdminHome _ -> reverse adminHomeR []
    AdminUsers _ -> reverse adminUsersR []

siteMap : Router Sitemap
siteMap = [homeR, usersR, userR, userEmailsR, userEmailR, deepR, customR] ++ (Route.map Admin adminRoutes)

homeR' = route Home (static "")
usersR' = route Users (static "users")
userR' = route User (prefix "users" int)
userEmailsR' = route UserEmails (prefix "users" int `and` static "emails")
userEmailR' = route UserEmail (prefix "users" int `and` prefix "emails" int)
deepR' = route Deep (prefix "deep" int `and` int `and` int)
customR' = route Custom' (prefix "custom" (custom fooP))

siteMap' : Router Sitemap
siteMap' = [homeR', usersR', userR', userEmailsR', userEmailR', deepR', customR'] ++ (Route.map Admin adminRoutes)
