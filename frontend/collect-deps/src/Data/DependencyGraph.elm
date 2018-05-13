module Data.DependencyGraph
    exposing
        ( DependencyContext
        , DependencyGraph
        , DependencyNode
        , NodeFilter
        , acceptAll
        , convertTree
        , decoder
        , groupArtifactFilter
        , groupArtifactVersionFilter
        , groupFilter
        )

import Data.Coordinate as Coord exposing (Coordinate)
import Data.Scope as Scope exposing (Scope)
import Data.Tree.Drawer as TD
import Graph exposing (Edge, Graph, Node, NodeContext, NodeId)
import Graph.Tree as Tree
import IntDict
import Json.Decode as Decode exposing (Decoder)


type alias DependencyGraph =
    Graph Coordinate Scope


type alias DependencyContext =
    NodeContext Coordinate Scope


type alias DependencyNode =
    Node Coordinate


type alias NodeFilter =
    DependencyNode -> Bool


acceptAll : NodeFilter
acceptAll =
    always True


groupFilter : String -> NodeFilter
groupFilter groupId node =
    node.label.groupId == groupId


groupArtifactFilter : String -> String -> NodeFilter
groupArtifactFilter groupId artifactId node =
    node.label.groupId == groupId && node.label.artifactId == artifactId


groupArtifactVersionFilter : String -> String -> String -> NodeFilter
groupArtifactVersionFilter groupId artifactId version node =
    node.label.artifactId == artifactId && node.label.groupId == groupId && node.label.version == version


{-| Take Tree produced by Graph.dfsTree and convert it to
rose tree which -for all non-root nodes - contains scope
-}
convertTree : DependencyGraph -> Maybe NodeId -> Tree.Tree DependencyContext -> TD.Tree ( DependencyNode, Maybe Scope )
convertTree graph mParentId t =
    case Tree.root t of
        Nothing ->
            TD.Node ( dummy, Nothing ) []

        Just ( ctx, children ) ->
            let
                maybeScope =
                    mParentId
                        |> Maybe.andThen (\parentId -> Graph.get parentId graph)
                        |> Maybe.andThen (\parentCtx -> IntDict.get ctx.node.id parentCtx.outgoing)
            in
            TD.Node
                ( ctx.node, maybeScope )
                (List.map (convertTree graph (Just ctx.node.id)) children)



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


{-| My Data.Tree implementation can't be empty, whereas Tree.Tree can be.
But since dependency trees are never empty (always contains at least the root artifact), it shouldn't be a problem.
This dummy is returned in the impossible case of empty dependency tree - to avoid Debug.crash
-}
dummy : DependencyNode
dummy =
    { id = -1
    , label =
        { groupId = ""
        , artifactId =
            "If you see this in the application, please report an issue in"
                ++ " https://github.com/jhrcek/kiegroup-poms-cleanup/issues"
                ++ " and include the URL where you saw it."
        , packaging = ""
        , qualifier = Nothing
        , version = ""
        , isOur = False
        }
    }
