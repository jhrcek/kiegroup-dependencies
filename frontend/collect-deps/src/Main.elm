module Main exposing (main)

import Data.DependencyGraph as DG exposing (DependencyGraph)
import Html exposing (Html, text)
import Http
import RemoteData exposing (RemoteData(..), WebData)
import Table
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
    { dependencyGraph : WebData DependencyGraph
    , tableState : Table.State
    }


type Msg
    = DependencyGraphLoaded (WebData DependencyGraph)
    | TableMsg Table.State


loadDependencyGraph : Cmd Msg
loadDependencyGraph =
    Http.get "/dependency-graph.json" DG.decoder
        |> RemoteData.sendRequest
        |> Cmd.map DependencyGraphLoaded


init : ( Model, Cmd Msg )
init =
    ( { dependencyGraph = RemoteData.Loading
      , tableState = Table.initialSort "ArtifactId"
      }
    , loadDependencyGraph
    )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    ( updatePure msg model, Cmd.none )


updatePure : Msg -> Model -> Model
updatePure msg model =
    case msg of
        DependencyGraphLoaded newDependencyGraph ->
            { model | dependencyGraph = newDependencyGraph }

        TableMsg newState ->
            { model | tableState = newState }


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
            DG.view TableMsg model.tableState (DG.getCoordinateList graph)
