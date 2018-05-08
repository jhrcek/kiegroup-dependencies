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


type alias DependencyGraph =
    Graph Coordinate Scope


type alias DependencyContext =
    NodeContext Coordinate Scope


convertTree : Tree.Tree DependencyContext -> TD.Tree DependencyContext
convertTree t =
    case Tree.root t of
        Nothing ->
            Debug.crash "This function can't deal with empty tree"

        Just ( ctx, children ) ->
            TD.Node ctx (List.map convertTree children)



-- JSON


decoder : Decoder DependencyGraph
decoder =
    Decode.map2 Graph.fromNodesAndEdges
        (Decode.field "nodes" (Decode.list nodeDecoder))
        (Decode.field "edges" (Decode.list edgeDecoder))


edgeDecoder : Decoder (Edge Scope)
edgeDecoder =
    Decode.map3 Edge
        (Decode.index 0 Decode.int)
        (Decode.index 1 Decode.int)
        (Decode.index 2 Scope.decoder)


nodeDecoder : Decoder (Node Coordinate)
nodeDecoder =
    Decode.map2 Node
        (Decode.index 0 Decode.int)
        (Decode.index 1 Coord.decoder)
