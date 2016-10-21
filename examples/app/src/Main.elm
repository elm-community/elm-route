module Main exposing (main)

import Data exposing (Post)
import Html as H exposing (Html)
import Html.Attributes as A
import Html.Events as E
import Http
import Json.Decode as JD
import Navigation exposing (Location)
import Routes exposing (Sitemap(..))
import Task


-- Update
-- ------


type alias Model =
    { route : Sitemap
    , ready : Bool
    , posts : List Post
    , post : Maybe Post
    , error : Maybe String
    }


type Msg
    = RouteChanged Sitemap
    | RouteTo Sitemap
    | Fetch (Result Http.Error (List Post))


parseRoute : Location -> Msg
parseRoute =
    Routes.parsePath >> RouteChanged


init : Location -> ( Model, Cmd Msg )
init location =
    let
        route =
            Routes.parsePath location
    in
        handleRoute route
            { route = route
            , ready = False
            , posts = []
            , post = Nothing
            , error = Nothing
            }


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        RouteChanged route ->
            handleRoute route model

        RouteTo route ->
            model ! [ Routes.navigateTo route ]

        Fetch (Err error) ->
            { model | error = Just (toString error) } ! []

        Fetch (Ok posts) ->
            handleRoute model.route
                { model
                    | ready = True
                    , error = Nothing
                    , posts = posts
                }


handleRoute : Sitemap -> Model -> ( Model, Cmd Msg )
handleRoute route ({ ready } as m) =
    let
        model =
            { m | route = route }
    in
        case route of
            PostsR ->
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
    Task.attempt Fetch Data.fetchPosts


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none



-- View
-- ----


notFound : Html Msg
notFound =
    H.h1 [] [ H.text "Page not found" ]


home : Html Msg
home =
    H.div []
        [ H.h1 [] [ H.text "Home Page" ]
        , H.p [] [ link (PostR 123) "This post does not exist" ]
        ]


about : Html Msg
about =
    H.h1 [] [ H.text "About" ]


loading : Html Msg
loading =
    H.h1 [] [ H.text "Loading..." ]


post : Data.Post -> Html Msg
post post =
    H.div []
        [ H.h1 [] [ H.text post.title ]
        , H.p [] [ H.text post.body ]
        ]


posts : List Data.Post -> Html Msg
posts posts =
    let
        postLink post =
            H.li [] [ link (PostR post.id) post.title ]
    in
        H.div []
            [ H.h1 [] [ H.text "Posts" ]
            , H.ul [] (List.map postLink posts)
            ]


view : Model -> Html Msg
view model =
    H.div []
        [ nav
        , content model
        ]


nav : Html Msg
nav =
    H.ul []
        [ H.li [] [ link HomeR "Home" ]
        , H.li [] [ link PostsR "Posts" ]
        , H.li [] [ link AboutR "About" ]
        ]


content : Model -> Html Msg
content ({ route } as model) =
    case model.route of
        HomeR ->
            home

        PostsR ->
            if model.ready then
                posts model.posts
            else
                loading

        PostR id ->
            case ( model.ready, model.post ) of
                ( False, _ ) ->
                    loading

                ( True, Nothing ) ->
                    notFound

                ( True, Just p ) ->
                    post p

        AboutR ->
            about

        NotFoundR ->
            notFound


link : Sitemap -> String -> Html Msg
link route label =
    let
        opts =
            { preventDefault = True, stopPropagation = True }
    in
        H.a
            [ A.href (Routes.toString route)
            , E.onWithOptions "click" opts (JD.succeed <| RouteTo route)
            ]
            [ H.text label ]



-- Main
-- ----


main : Program Never Model Msg
main =
    Navigation.program parseRoute
        { init = init
        , update = update
        , view = view
        , subscriptions = subscriptions
        }
