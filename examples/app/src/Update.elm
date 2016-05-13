module Update exposing (Flags, Msg(..), Model, init, update)

import Data exposing (Post, fetchPosts)
import Http
import Routes exposing (Sitemap(..))
import Ports exposing (pushPath)
import Task


type alias Flags =
    { path : String
    }


type alias Model =
    { route : Sitemap
    , ready : Bool
    , posts : List Post
    , post : Maybe Post
    , error : Maybe String
    }


type Msg
    = PathChanged String
    | RouteTo Sitemap
    | FetchError Http.Error
    | FetchSuccess (List Post)


handleRoute : Model -> ( Model, Cmd Msg )
handleRoute ({ route, ready } as model) =
    let
        fetchPosts =
            Task.perform FetchError FetchSuccess Data.fetchPosts
    in
        case route of
            PostsR () ->
                if ready then
                    ( model, Cmd.none )
                else
                    ( model, fetchPosts )

            PostR id ->
                if ready then
                    ( { model | post = Data.lookupPost id model.posts }, Cmd.none )
                else
                    ( model, fetchPosts )

            _ ->
                ( model, Cmd.none )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        PathChanged path ->
            handleRoute { model | route = Routes.match path }

        RouteTo route ->
            ( model, pushPath (Routes.route route) )

        FetchError error ->
            ( { model | error = Just (toString error) }, Cmd.none )

        FetchSuccess posts ->
            handleRoute { model | ready = True, error = Nothing, posts = posts }


init : Flags -> ( Model, Cmd Msg )
init { path } =
    handleRoute
        { route = Routes.match path
        , ready = False
        , posts = []
        , post = Nothing
        , error = Nothing
        }
