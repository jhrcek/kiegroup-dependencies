module Data.DependencyGraph exposing (DependencyGraph, decoder)

import Data.Coordinate as Coord exposing (Coordinate)
import Graph exposing (Edge, Graph, Node)
import Json.Decode as Decode exposing (Decoder)


type alias DependencyGraph =
    Graph Coordinate String


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
