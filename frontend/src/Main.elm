module Main exposing (main)

import Data.Coordinate as Coordinate exposing (Coordinate)
import Data.Tree as Tree exposing (Tree)
import Html exposing (Html, text)
import Http
import Navigation
import RemoteData exposing (WebData)
import Route


main : Program Never Model Msg
main =
    Navigation.program
        UrlChanged
        { init = init
        , update = update
        , view = view
        , subscriptions = \_ -> Sub.none
        }


type Page
    = Home


type Msg
    = UrlChanged Navigation.Location
    | ModuleTreeLoaded (WebData ModuleTree)


type alias ModuleTree =
    Tree Coordinate


loadData : Cmd Msg
loadData =
    Http.get "/module-structure.json" (Tree.decoder Coordinate.decoder)
        |> RemoteData.sendRequest
        |> Cmd.map ModuleTreeLoaded


type alias Flags =
    List String


type alias Model =
    { moduleTree : WebData ModuleTree
    , page : Page
    }


init : Navigation.Location -> ( Model, Cmd Msg )
init location =
    ( setRoute (Route.fromLocation location)
        { moduleTree = RemoteData.Loading
        , page = Home
        }
    , loadData
    )


setRoute : Maybe Route.Route -> Model -> Model
setRoute mRoute model =
    let
        _ =
            Debug.log "Parsed Url " mRoute
    in
    model


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        UrlChanged newUrl ->
            ( model, Cmd.none )

        ModuleTreeLoaded mt ->
            ( { model | moduleTree = mt }, Cmd.none )


view : Model -> Html Msg
view model =
    case model.page of
        Home ->
            viewHome model.moduleTree


viewHome : WebData ModuleTree -> Html Msg
viewHome x =
    Html.div []
        [ text <| toString x ]
