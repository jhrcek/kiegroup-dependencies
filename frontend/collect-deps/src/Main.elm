module Main exposing (main)

import Data.Coordinate as Coord exposing (Coordinate)
import Data.DependencyGraph as DepGraph exposing (DependencyContext, DependencyGraph, NodeFilter)
import Data.DependencyTree as DepTree
import Data.Scope exposing (Scope)
import Graph exposing (Adjacency)
import Graph.Tree as Tree
import Html exposing (Html, a, button, div, h1, h2, text)
import Html.Attributes exposing (href)
import Html.Events exposing (onClick)
import Http
import IntDict
import Navigation
import Page exposing (Page(..))
import Page.DependencyConvergence as Convergence
import RemoteData exposing (RemoteData(..), WebData)
import Table
import View.DependencyTable as DepTable
import View.DependencyTree as DepTree


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
    , transitiveConfig : TransitiveConfig
    , tableState : Table.State
    , page : Page
    }


type alias TransitiveConfig =
    { showForward : Bool
    , showReverse : Bool
    }


type Msg
    = DependencyGraphLoaded (WebData DependencyGraph)
    | SortTable Table.State
    | LocationChanged Navigation.Location
    | ShowForwardTransitive Bool
    | ShowReverseTransitive Bool


loadDependencyGraph : Cmd Msg
loadDependencyGraph =
    Http.get "dependency-graph.json" DepGraph.decoder
        |> RemoteData.sendRequest
        |> Cmd.map DependencyGraphLoaded


init : Navigation.Location -> ( Model, Cmd Msg )
init location =
    ( { dependencyGraph = RemoteData.Loading
      , transitiveConfig =
            { showForward = True
            , showReverse = True
            }
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

        ShowForwardTransitive bool ->
            { model | transitiveConfig = setShowForwardTransitive bool model.transitiveConfig }

        ShowReverseTransitive bool ->
            { model | transitiveConfig = setShowReverseTransitive bool model.transitiveConfig }


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
                    viewArtifactTable model.tableState graph DepGraph.acceptAll

                DependencyConvergence ->
                    Convergence.view graph

                Group groupId ->
                    viewArtifactTable model.tableState graph (DepGraph.groupFilter groupId)

                GroupArtifact groupId artifactId ->
                    viewArtifactTable model.tableState graph (DepGraph.groupArtifactFilter groupId artifactId)

                GroupArtifactVersion groupId artifactId version ->
                    viewArtifactTable model.tableState graph (DepGraph.groupArtifactVersionFilter groupId artifactId version)

                CoordinateDetails nodeId ->
                    case Graph.get nodeId graph of
                        Nothing ->
                            text <| "There is no coordinate with nodeId " ++ toString nodeId

                        Just nodeContext ->
                            viewDependencyDetails model.transitiveConfig nodeContext graph
    in
    div [] [ breadcrumb, contents ]


setShowForwardTransitive : Bool -> TransitiveConfig -> TransitiveConfig
setShowForwardTransitive show c =
    { c | showForward = show }


setShowReverseTransitive : Bool -> TransitiveConfig -> TransitiveConfig
setShowReverseTransitive show c =
    { c | showReverse = show }


viewArtifactTable : Table.State -> DependencyGraph -> NodeFilter -> Html Msg
viewArtifactTable tableState graph nodeFilter =
    DepTable.view
        SortTable
        tableState
        (Graph.nodes graph |> List.filter nodeFilter)


viewDependencyDetails : TransitiveConfig -> DependencyContext -> DependencyGraph -> Html Msg
viewDependencyDetails transitiveConfig ctx graph =
    let
        coordinate =
            ctx.node.label

        revGraph =
            Graph.reverseEdges graph
    in
    div []
        [ h1 [] [ text (Coord.toString coordinate) ]
        , mavenCentralLink coordinate
        , dependencyTreeView
            "Dependencies"
            graph
            ctx
            ctx.outgoing
            transitiveConfig.showForward
            ShowForwardTransitive
        , dependencyTreeView
            "Reverse dependencies"
            revGraph
            ctx
            ctx.incoming
            transitiveConfig.showReverse
            ShowReverseTransitive
        ]


dependencyTreeView : String -> DependencyGraph -> DependencyContext -> Adjacency Scope -> Bool -> (Bool -> Msg) -> Html Msg
dependencyTreeView heading graph depCtx directChildren showTransitive transitToggleMsg =
    let
        depTree : Tree.Tree DependencyContext
        depTree =
            Graph.dfsTree depCtx.node.id graph

        viewCounts : Int -> Int -> Html msg
        viewCounts directCount transitiveCount =
            text <| toString directCount ++ " direct, " ++ toString transitiveCount ++ " transitive "

        transitiveControlButton =
            button [ onClick (transitToggleMsg (not showTransitive)) ]
                [ text <|
                    (if showTransitive then
                        "Hide"
                     else
                        "Show"
                    )
                        ++ " transitive"
                ]

        transitiveDepsCount =
            Tree.size depTree - 1
    in
    div []
        [ h2 [] [ text heading ]
        , viewCounts (IntDict.size directChildren) transitiveDepsCount
        , viewIf (transitiveDepsCount > 0) transitiveControlButton
        , DepTree.view <|
            if showTransitive then
                DepTree.buildTransitiveDependencyTree graph depTree
            else
                DepTree.buildDirectDependencyTree graph depCtx.node directChildren
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


viewIf : Bool -> Html a -> Html a
viewIf test html =
    if test then
        html
    else
        text ""
