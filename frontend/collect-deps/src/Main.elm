module Main exposing (main)

import Data.Coordinate as Coordinate exposing (Coordinate)
import Data.DependencyGraph as DG exposing (DependencyGraph)
import Html exposing (Html, text)
import Http
import RemoteData exposing (RemoteData(..), WebData)
import View.DependencyGraph as DG


main : Program Never Model Msg
main =
    Html.program
        { init = init
        , update = update
        , view = view
        , subscriptions = \_ -> Sub.none
        }


type alias Model =
    { dependencyGraph : WebData DependencyGraph }


type Msg
    = DependencyGraphLoaded (WebData DependencyGraph)


loadDependencyGraph : Cmd Msg
loadDependencyGraph =
    Http.get "/dependency-graph.json" DG.decoder
        |> RemoteData.sendRequest
        |> Cmd.map DependencyGraphLoaded


init : ( Model, Cmd Msg )
init =
    ( { dependencyGraph = RemoteData.Loading }, loadDependencyGraph )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        DependencyGraphLoaded dg ->
            ( { model | dependencyGraph = dg }, Cmd.none )


view : Model -> Html Msg
view model =
    case model.dependencyGraph of
        NotAsked ->
            text "not asked"

        Loading ->
            text "loading"

        Failure e ->
            text <| toString e

        Success graph ->
            DG.view graph
