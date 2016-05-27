module Update exposing (Msg(..), Model, init, update, urlUpdate)

import Data exposing (Post, fetchPosts)
import Http
import Navigation exposing (Location)
import Routes exposing (Sitemap(..))
import Task


type alias Model =
    { route : Sitemap
    , ready : Bool
    , posts : List Post
    , post : Maybe Post
    , error : Maybe String
    }


type Msg
    = RouteTo Sitemap
    | FetchError Http.Error
    | FetchSuccess (List Post)


init : Sitemap -> ( Model, Cmd Msg )
init route =
    urlUpdate route
        { route = route
        , ready = False
        , posts = []
        , post = Nothing
        , error = Nothing
        }


update : Msg -> Model -> ( Model, Cmd Msg )
update msg ({ route } as model) =
    case msg of
        RouteTo route ->
            model ! [ Routes.navigateTo route ]

        FetchError error ->
            { model | error = Just (toString error) } ! []

        FetchSuccess posts ->
            urlUpdate route { model | ready = True, error = Nothing, posts = posts }


urlUpdate : Sitemap -> Model -> ( Model, Cmd Msg )
urlUpdate route ({ ready } as m) =
    let
        model =
            { m | route = route }
    in
        case route of
            PostsR () ->
                if ready then
                    model ! []
                else
                    model ! [ fetchPosts ]

            PostR id ->
                if ready then
                    { model | post = Data.lookupPost id model.posts } ! []
                else
                    model ! [ fetchPosts ]

            _ ->
                model ! []


fetchPosts : Cmd Msg
fetchPosts =
    Task.perform FetchError FetchSuccess Data.fetchPosts
