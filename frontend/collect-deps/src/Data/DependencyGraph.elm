module Data.DependencyGraph
    exposing
        ( BackendDependencyGraph
        , DependencyContext
        , DependencyGraph
        , DependencyNode
        , NodeFilter
        , acceptAll
        , calculateFrontentData
        , decoder
        , groupArtifactFilter
        , groupArtifactVersionFilter
        , groupFilter
        , ourGroupIds
        )

import Array exposing (Array)
import Data.Coordinate as Coord exposing (BackendCoordinate, Coordinate)
import Data.Scope as Scope exposing (Scope)
import Graph exposing (Adjacency, Edge, Graph, Node, NodeContext, NodeId)
import IntDict
import Json.Decode as Decode exposing (Decoder)
import Set exposing (Set)


type alias BackendDependencyGraph =
    Graph BackendCoordinate Scope


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


calculateFrontentData : BackendDependencyGraph -> DependencyGraph
calculateFrontentData backendDependencyGraph =
    let
        depCounts =
            countDependencies backendDependencyGraph

        addInfoToContext : NodeContext BackendCoordinate e -> NodeContext Coordinate e
        addInfoToContext ctx =
            { ctx | node = addInfoToNode ctx.node }

        addInfoToNode : Node BackendCoordinate -> Node Coordinate
        addInfoToNode backendNode =
            { id = backendNode.id
            , label = addInfo backendNode.id backendNode.label
            }

        addInfo : NodeId -> BackendCoordinate -> Coordinate
        addInfo id backendCoord =
            { groupId = backendCoord.groupId
            , artifactId = backendCoord.artifactId
            , packaging = backendCoord.packaging
            , qualifier = backendCoord.qualifier
            , version = backendCoord.version
            , isOur = isOurGroupId backendCoord.groupId
            , directDepsCount = Array.get id depCounts.direct |> Maybe.withDefault 0
            , transitiveDepsCount = Array.get id depCounts.transitive |> Maybe.withDefault 0
            , reverseDirectDepsCount = Array.get id depCounts.reverseDirect |> Maybe.withDefault 0
            , reverseTransitiveDepsCount = Array.get id depCounts.reverseTransitive |> Maybe.withDefault 0
            }
    in
    Graph.mapContexts addInfoToContext backendDependencyGraph


isOurGroupId : String -> Bool
isOurGroupId testedGroupId =
    List.any (\ourGroupId -> String.startsWith ourGroupId testedGroupId) ourGroupIds


ourGroupIds : List String
ourGroupIds =
    [ "org.kie"
    , "org.drools"
    , "org.jbpm"
    , "org.uberfire"
    , "org.dashbuilder"
    , "org.optaplanner"
    ]


type alias DependencyCounts =
    { direct : Array Int
    , transitive : Array Int
    , reverseDirect : Array Int
    , reverseTransitive : Array Int
    }


emptyCounts : DependencyCounts
emptyCounts =
    { direct = Array.empty
    , transitive = Array.empty
    , reverseDirect = Array.empty
    , reverseTransitive = Array.empty
    }


{-| Function choosing either outgoing or incoming edges
-}
type alias DirectionSelector =
    NodeContext BackendCoordinate Scope -> Adjacency Scope


{-| This is very expensive operation expected to be done only once - at app initialization!
-}
countDependencies : BackendDependencyGraph -> DependencyCounts
countDependencies graph =
    let
        -- folding function takes context and unions all its direct dependency ids
        -- with all indirect dependency ids
        processNodeContext :
            DirectionSelector
            -> NodeContext BackendCoordinate Scope
            -> Array (Set Int)
            -> Array (Set Int)
        processNodeContext direction nodeContext artId_to_depIds =
            let
                directDepIds =
                    IntDict.keys <| direction nodeContext

                transitiveDepsSet =
                    List.filterMap (\depId -> Array.get depId artId_to_depIds) directDepIds
                        |> List.foldl Set.union (Set.fromList directDepIds)
            in
            Array.set nodeContext.node.id transitiveDepsSet artId_to_depIds

        countDirect : BackendDependencyGraph -> DirectionSelector -> Array Int
        countDirect gr direction =
            Graph.fold
                (\ctx arr -> Array.set ctx.node.id (IntDict.size (direction ctx)) arr)
                (Array.repeat (Graph.size gr) 0)
                gr
    in
    case Graph.checkAcyclic graph of
        Ok acyclicGraph ->
            { direct = countDirect graph .outgoing

            -- process the nodes in order of reverse topological sort, so that at the point when
            -- folding function processes node x, all its dependencies are already processed
            , transitive =
                Graph.topologicalSort acyclicGraph
                    |> List.foldr (processNodeContext .outgoing) (Array.repeat (Graph.size graph) Set.empty)
                    |> Array.map (\depSet -> Set.size depSet)
            , reverseDirect = countDirect graph .incoming

            -- process in topological order in order to get reverse deps
            , reverseTransitive =
                Graph.topologicalSort acyclicGraph
                    |> List.foldl (processNodeContext .incoming) (Array.repeat (Graph.size graph) Set.empty)
                    |> Array.map (\depSet -> Set.size depSet)
            }

        Err edgesFormingCycle ->
            Debug.log
                ("The dependency graph was not acyclic! The following edges form a cycle: " ++ toString edgesFormingCycle)
                emptyCounts



-- JSON


decoder : Decoder BackendDependencyGraph
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


nodeDecoder : Decoder (Node BackendCoordinate)
nodeDecoder =
    Decode.map2 Node
        (Decode.index 0 Decode.int)
        (Decode.index 1 Coord.decoder)
