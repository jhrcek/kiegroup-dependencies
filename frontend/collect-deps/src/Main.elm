module Main exposing (main)

import Data.Coordinate as Coord exposing (Coordinate)
import Data.DependencyGraph as DG exposing (DependencyContext, DependencyGraph)
import Graph as G exposing (Adjacency, Node, NodeId)
import Graph.Tree as Tree
import Html exposing (Html, a, div, h1, h2, li, table, td, text, tr, ul)
import Html.Attributes exposing (href)
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
    = Summary
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
      , page = Summary
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
        Summary ->
            div []
                [ DG.view
                    SortTable
                    (PageChange << DependencyDetails)
                    model.tableState
                    (DG.getCoordinateNodes graph)
                ]

        DependencyDetails nodeId ->
            case G.get nodeId graph of
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
    in
    div []
        [ h1 [] [ text (Coord.toString coordinate) ]
        , table []
            [ tr []
                [ td [] [ text "Direct dependencies" ]
                , td [] [ text <| toString <| IntDict.size out ]
                ]
            , tr []
                [ td [] [ text "Transitive dependencies" ]
                , td [] [ text <| toString <| Tree.size <| G.dfsTree ctx.node.id graph ]
                ]
            , tr []
                [ td [] [ text "Direct dependees" ]
                , td [] [ text <| toString <| IntDict.size inc ]
                ]
            , tr []
                [ td [] [ text "Transitive dependees" ]
                , td [] [ text <| toString <| Tree.size <| G.dfsTree ctx.node.id <| G.reverseEdges graph ]
                ]
            ]
        , h2 [] [ text "Direct dependencies" ]
        , viewAdjacentCoordinates out graph
        , h2 [] [ text "Direct dependees" ]
        , viewAdjacentCoordinates inc graph
        , a [ href "#", onClick (PageChange Summary) ] [ text "Back to summary" ]
        ]


viewAdjacentCoordinates : Adjacency String -> DependencyGraph -> Html Msg
viewAdjacentCoordinates adj graph =
    IntDict.toList adj
        |> List.filterMap
            (\( nid, _ {- <- TODO use scope -} ) ->
                G.get nid graph
                    |> Maybe.map (\adjCtx -> viewCoord adjCtx.node)
            )
        |> ul []


viewCoord : Node Coordinate -> Html Msg
viewCoord node =
    li []
        [ a [ href "#", onClick (PageChange <| DependencyDetails node.id) ]
            [ text <| Coord.toString node.label ]
        ]
