module Data.DependencyGraph
    exposing
        ( DependencyContext
        , DependencyGraph
        , convertTree
        , decoder
        )

import Data.Coordinate as Coord exposing (Coordinate)
import Data.Tree.Drawer as TD
import Graph exposing (Edge, Graph, Node, NodeContext)
import Graph.Tree as Tree
import Json.Decode as Decode exposing (Decoder)


type alias DependencyGraph =
    Graph Coordinate String


type alias DependencyContext =
    NodeContext Coordinate String


convertTree : Tree.Tree DependencyContext -> TD.Tree String
convertTree t =
    case Tree.root t of
        Nothing ->
            TD.Node "EMPTY" []

        Just ( ctx, children ) ->
            TD.Node (Coord.toString ctx.node.label) (List.map convertTree children)



-- JSON


decoder : Decoder DependencyGraph
decoder =
    Decode.map2 Graph.fromNodesAndEdges
        (Decode.field "nodes" (Decode.list nodeDecoder))
        (Decode.field "edges" (Decode.list edgeDecoder))


edgeDecoder : Decoder (Edge String)
edgeDecoder =
    Decode.map3 Edge
        (Decode.index 0 Decode.int)
        (Decode.index 1 Decode.int)
        (Decode.index 2 Decode.string)


nodeDecoder : Decoder (Node Coordinate)
nodeDecoder =
    Decode.map2 Node
        (Decode.index 0 Decode.int)
        (Decode.index 1 Coord.decoder)
