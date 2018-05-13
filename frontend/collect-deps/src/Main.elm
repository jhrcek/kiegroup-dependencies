module Main exposing (main)

import Data.Coordinate as Coord exposing (Coordinate)
import Data.DependencyGraph as DG exposing (DependencyContext, DependencyGraph, NodeFilter)
import Graph exposing (Graph)
import Graph.Tree as Tree
import Html exposing (Html, a, div, h1, h2, span, text)
import Html.Attributes exposing (href, style)
import Http
import IntDict
import Navigation
import Page exposing (Page(..))
import Page.DependencyConvergence as Convergence
import RemoteData exposing (RemoteData(..), WebData)
import Table
import View.DependencyGraph as DG


main : Program Never Model Msg
main =
    Navigation.program LocationChanged
        { init = init
        , update = update
        , view = view
        , subscriptions = \_ -> Sub.none
        }


type alias Model =
    { dependencyGraph : WebData DependencyGraph
    , tableState : Table.State
    , page : Page
    }


type Msg
    = DependencyGraphLoaded (WebData DependencyGraph)
    | SortTable Table.State
    | LocationChanged Navigation.Location


loadDependencyGraph : Cmd Msg
loadDependencyGraph =
    Http.get "dependency-graph.json" DG.decoder
        |> RemoteData.sendRequest
        |> Cmd.map DependencyGraphLoaded


init : Navigation.Location -> ( Model, Cmd Msg )
init location =
    ( { dependencyGraph = RemoteData.Loading
      , tableState = Table.initialSort "ArtifactId"
      , page = Page.parseLocation location
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

        SortTable newState ->
            { model | tableState = newState }

        LocationChanged location ->
            { model | page = Page.parseLocation location }


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
            viewPage model graph


viewPage : Model -> DependencyGraph -> Html Msg
viewPage model graph =
    let
        breadcrumb =
            Page.toBreadCrumb model.page graph

        contents =
            case model.page of
                Home ->
                    viewArtifactTable model.tableState graph DG.acceptAll

                DependencyConvergence ->
                    Convergence.view graph

                Group groupId ->
                    viewArtifactTable model.tableState graph (DG.groupFilter groupId)

                GroupArtifact groupId artifactId ->
                    viewArtifactTable model.tableState graph (DG.groupArtifactFilter groupId artifactId)

                GroupArtifactVersion groupId artifactId version ->
                    viewArtifactTable model.tableState graph (DG.groupArtifactVersionFilter groupId artifactId version)

                CoordinateDetails nodeId ->
                    case Graph.get nodeId graph of
                        Nothing ->
                            text <| "There is no coordinate with nodeId " ++ toString nodeId

                        Just nodeContext ->
                            viewDependencyDetails nodeContext graph
    in
    div [] [ breadcrumb, contents ]


viewArtifactTable : Table.State -> Graph Coordinate e -> NodeFilter -> Html Msg
viewArtifactTable tableState graph nodeFilter =
    div []
        [ DG.view
            SortTable
            tableState
            (Graph.nodes graph |> List.filter nodeFilter)
        ]


viewDependencyDetails : DependencyContext -> DependencyGraph -> Html Msg
viewDependencyDetails ctx graph =
    let
        coordinate =
            ctx.node.label

        inc =
            ctx.incoming

        out =
            ctx.outgoing

        revGraph =
            Graph.reverseEdges graph

        depTree =
            Graph.dfsTree ctx.node.id graph

        reverseDepTree =
            Graph.dfsTree ctx.node.id revGraph

        additionalInfo direct transitive =
            span [ style [ ( "font-size", "16px" ) ] ]
                [ text <| " (" ++ toString direct ++ " direct, " ++ toString transitive ++ " transitive)" ]
    in
    div []
        [ h1 [] [ text (Coord.toString coordinate) ]
        , mavenCentralLink coordinate
        , h2 [] [ text "Dependencies", additionalInfo (IntDict.size out) (Tree.size depTree - 1) ]
        , DG.dependencyTreeView depTree graph
        , h2 [] [ text "Reverse dependencies", additionalInfo (IntDict.size inc) (Tree.size reverseDepTree - 1) ]
        , DG.dependencyTreeView reverseDepTree revGraph
        ]


mavenCentralLink : Coordinate -> Html a
mavenCentralLink coordinate =
    if String.endsWith "SNAPSHOT" coordinate.version then
        text ""
    else
        a [ href <| mavenCentralUrl coordinate ] [ text "Maven central" ]


mavenCentralUrl : Coordinate -> String
mavenCentralUrl { groupId, artifactId, version } =
    String.join "/" [ "https://mvnrepository.com/artifact", groupId, artifactId, version ]
