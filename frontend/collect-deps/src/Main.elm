module Main exposing (main)

import Data.Coordinate as Coord exposing (Coordinate)
import Data.DependencyGraph as DG exposing (DependencyContext, DependencyGraph)
import Graph exposing (NodeId)
import Graph.Tree as Tree
import Html exposing (Html, a, div, h1, h2, li, span, text, ul)
import Html.Attributes exposing (href, style)
import Html.Events exposing (onClick)
import Http
import IntDict
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
    , page : Page
    }


type Page
    = Home
    | DependencyDetails NodeId


type Msg
    = DependencyGraphLoaded (WebData DependencyGraph)
    | SortTable Table.State
    | PageChange Page


loadDependencyGraph : Cmd Msg
loadDependencyGraph =
    Http.get "/dependency-graph.json" DG.decoder
        |> RemoteData.sendRequest
        |> Cmd.map DependencyGraphLoaded


init : ( Model, Cmd Msg )
init =
    ( { dependencyGraph = RemoteData.Loading
      , tableState = Table.initialSort "ArtifactId"
      , page = Home
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

        PageChange newPage ->
            { model | page = newPage }


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
    case model.page of
        Home ->
            div []
                [ DG.view
                    SortTable
                    (PageChange << DependencyDetails)
                    model.tableState
                    (Graph.nodes graph)
                ]

        DependencyDetails nodeId ->
            case Graph.get nodeId graph of
                Nothing ->
                    text <| "There is no node with nodeId " ++ toString nodeId

                Just nodeContext ->
                    viewDependencyDetails nodeContext graph


viewDependencyDetails : DependencyContext -> DependencyGraph -> Html Msg
viewDependencyDetails ctx graph =
    let
        coordinate =
            ctx.node.label

        inc =
            ctx.incoming

        out =
            ctx.outgoing

        depTree =
            Graph.dfsTree ctx.node.id graph

        reverseDepTree =
            Graph.dfsTree ctx.node.id <| Graph.reverseEdges graph

        additionalInfo direct transitive =
            span [ style [ ( "font-size", "16px" ) ] ] [ text <| " (" ++ toString direct ++ " direct, " ++ toString transitive ++ " transitive)" ]
    in
    div []
        [ h1 [] [ text (Coord.toString coordinate) ]
        , h2 [] [ text "Dependencies", additionalInfo (IntDict.size out) (Tree.size depTree - 1) ]
        , DG.dependencyTreeView (PageChange << DependencyDetails) depTree
        , h2 [] [ text "Reverse dependencies", additionalInfo (IntDict.size inc) (Tree.size reverseDepTree - 1) ]
        , DG.dependencyTreeView (PageChange << DependencyDetails) reverseDepTree
        , h2 [] [ text "Links" ]
        , ul []
            [ li [] [ a [ href "#", onClick (PageChange Home) ] [ text "Home" ] ]
            , li [] [ mavenCentralLink coordinate ]
            ]
        ]


mavenCentralLink : Coordinate -> Html a
mavenCentralLink coordinate =
    let
        url =
            String.join "/" [ "https://mvnrepository.com/artifact", coordinate.groupId, coordinate.artifactId, coordinate.version ]
    in
    if String.endsWith "SNAPSHOT" coordinate.version then
        text "Maven central link not available for SNAPSHOT artifacts"
    else
        a [ href url ] [ text "Maven central" ]
