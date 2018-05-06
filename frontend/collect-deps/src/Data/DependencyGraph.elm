module Data.DependencyGraph
    exposing
        ( DependencyContext
        , DependencyGraph
        , convertTree
        , decoder
        )

import Data.Coordinate as Coord exposing (Coordinate)
import Data.Scope as Scope exposing (Scope)
import Data.Tree.Drawer as TD
import Graph exposing (Edge, Graph, Node, NodeContext)
import Graph.Tree as Tree
import Json.Decode as Decode exposing (Decoder)
import Set exposing (Set)


type alias DependencyGraph =
    Graph Coordinate Scope


type alias DependencyContext =
    NodeContext Coordinate Scope


convertTree : Tree.Tree DependencyContext -> TD.Tree String
convertTree t =
    case Tree.root t of
        Nothing ->
            TD.Node "EMPTY" []

        Just ( ctx, children ) ->
            TD.Node
                (Coord.toString ctx.node.label)
                (List.map convertTree children)



-- JSON


decoder : Decoder DependencyGraph
decoder =
    ourCoordinateIdsDecoder
        |> Decode.andThen
            (\ourCoordinateIds ->
                Decode.map2 Graph.fromNodesAndEdges
                    (Decode.field "nodes" (Decode.list (nodeDecoder ourCoordinateIds)))
                    (Decode.field "edges" (Decode.list edgeDecoder))
            )


ourCoordinateIdsDecoder : Decoder (Set Int)
ourCoordinateIdsDecoder =
    Decode.field "ourCoordinateIds" (Decode.list Decode.int)
        |> Decode.map Set.fromList


edgeDecoder : Decoder (Edge Scope)
edgeDecoder =
    Decode.map3 Edge
        (Decode.index 0 Decode.int)
        (Decode.index 1 Decode.int)
        (Decode.index 2 Scope.decoder)


nodeDecoder : Set Int -> Decoder (Node Coordinate)
nodeDecoder ourCoordinateIds =
    Decode.index 0 Decode.int
        |> Decode.andThen
            (\coordinateId ->
                let
                    is3rdParty =
                        not <| Set.member coordinateId ourCoordinateIds
                in
                Decode.map
                    (Node coordinateId)
                    (Decode.index 1 (Coord.decoder is3rdParty))
            )
