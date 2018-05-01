module Data.DependencyGraph
    exposing
        ( DependencyContext
        , DependencyGraph
        , decoder
        , getCoordinateNodes
        )

import Data.Coordinate as Coord exposing (Coordinate)
import Graph exposing (Edge, Graph, Node, NodeContext)
import Json.Decode as Decode exposing (Decoder)


type alias DependencyGraph =
    Graph Coordinate String


type alias DependencyContext =
    NodeContext Coordinate String


getCoordinateNodes : DependencyGraph -> List (Node Coordinate)
getCoordinateNodes =
    Graph.nodes



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
