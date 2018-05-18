module Main exposing (main)

import Data.Coordinate as Coord exposing (Coordinate)
import Data.DependencyGraph as DepGraph exposing (BackendDependencyGraph, DependencyContext, DependencyGraph, NodeFilter)
import Data.DependencyTree as DepTree
import Data.Scope exposing (Scope)
import Graph exposing (Adjacency)
import Graph.Tree as Tree
import Html exposing (Html, a, button, div, h1, h2, text)
import Html.Attributes exposing (class, href, target)
import Html.Events exposing (onClick)
import Http
import IntDict
import Markdown
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
    = DependencyGraphLoaded (WebData BackendDependencyGraph)
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
            let
                enrichedDepGraph =
                    RemoteData.map DepGraph.calculateFrontentData newDependencyGraph
            in
            { model | dependencyGraph = enrichedDepGraph }

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
            div [ class "spinner" ] []

        Failure e ->
            text <| toString e

        Success graph ->
            viewPage model graph


viewPage : Model -> DependencyGraph -> Html Msg
viewPage model graph =
    let
        content =
            case model.page of
                AllArtifacts ->
                    viewArtifactTable model.tableState graph DepGraph.acceptAll

                Help ->
                    viewHelp

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
    div []
        [ Page.navigation model.page graph
        , div [ class "content" ]
            [ content ]
        ]


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
        a [ href <| mavenCentralUrl coordinate, target "_blank" ] [ text "Maven central" ]


mavenCentralUrl : Coordinate -> String
mavenCentralUrl { groupId, artifactId, version } =
    String.join "/" [ "https://mvnrepository.com/artifact", groupId, artifactId, version ]


viewIf : Bool -> Html a -> Html a
viewIf test html =
    if test then
        html
    else
        text ""


viewHelp : Html a
viewHelp =
    Markdown.toHtml [] <|
        """
# What is this?

An interactive report showing all artifacts built from [kiegroup](https://github.com/kiegroup) repositories.
All the artifacts that are part of `droolsjbpm-build-bootstrap/scripts/mvn-all.sh clean install` build
are included, as well as their transitive dependencies.

# What does DDs, TDs, RDs and RTDs mean?

- DDs - number of **D**irect **D**ependencies of the artifact
- TDs - number of **T**ransitive **D**ependencies of the artifact. This includes direct dependencies, their dependencies and so on recursively.
- RDs - number of **R**everse **D**ependencies of the artifact. Reverse dependency of given artifact has this artifact as direct dependency.
- RTDs - number of **R**everse **T**ransitive **D**ependencies of the artifact. This is dual of TDs - all artifacts that directly or transitively depend on this artifact.

The following picture illustrates the concepts (arrow x -> y represents the fact that y is direct dependency of x):

![Example explaining what DDs, TDs, RDs and RTDs mean](img/dep-example.png "Example explaining what DDs, TDs, RDs and RTDs mean")

# What do the blue rows mean?

The blue rows highlight artifacts, whose sources come (most likely) from kiegroup repositories.
They are recognized heuristically by groupId starting with one of the following prefixes:
"""
            ++ String.join "\n" (List.map (\g -> "- " ++ g) DepGraph.ourGroupIds)
            ++ """

# Where do the data come from?

The data are based on the reports produced by

```bash
./droolsjbpm-build-bootstrap/script/mvn-all.sh dependency:tree -DoutputType=tgf -DoutputFile=deps.tgf -DfullProfile
```

That command produces about 900 [TGF](https://en.wikipedia.org/wiki/Trivial_Graph_Format) files (1 for each maven module).
Each of these represents dependency tree of one artifact.
Using a script, these files are merged into [dependency-graph.json](dependency-graph.json).
The nodes of the graph correspond to maven coordinates and edges represent relation of direct dependency among artifacts.
This website is a single page application for browsing this graph.

# Can you add feature XYZ?

Feel free to [open an issue](https://github.com/jhrcek/kiegroup-dependencies/issues) if you have ideas how to make this report better or more useful.
"""
