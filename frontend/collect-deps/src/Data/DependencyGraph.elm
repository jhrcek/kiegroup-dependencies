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
        )

import Data.Coordinate as Coord exposing (BackendCoordinate, Coordinate)
import Data.Scope as Scope exposing (Scope)
import Graph exposing (Adjacency, Edge, Graph, Node, NodeContext, NodeId)
import IntDict exposing (IntDict)
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
        { transitiveCounts, reverseTransitiveCounts } =
            countTransitiveDependencies backendDependencyGraph

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
            , transitiveDepsCount = IntDict.get id transitiveCounts |> Maybe.withDefault 0
            , reverseTransitiveDepsCount = IntDict.get id reverseTransitiveCounts |> Maybe.withDefault 0
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
    { transitiveCounts : IntDict Int
    , reverseTransitiveCounts : IntDict Int
    }


{-| This is very expensive operation expected to be done only once - at app initialization!
-}
countTransitiveDependencies : BackendDependencyGraph -> DependencyCounts
countTransitiveDependencies graph =
    let
        -- folding function takes context and unions all its direct dependency ids
        -- with all indirect dependency ids
        processNodeContext :
            (NodeContext BackendCoordinate Scope -> Adjacency Scope)
            -> NodeContext BackendCoordinate Scope
            -> IntDict (Set Int)
            -> IntDict (Set Int)
        processNodeContext direction nodeContext artId_to_depIds =
            let
                directDepIds =
                    IntDict.keys <| direction nodeContext

                transitiveDepsSet =
                    List.filterMap (\depId -> IntDict.get depId artId_to_depIds) directDepIds
                        |> List.foldl Set.union (Set.fromList directDepIds)
            in
            IntDict.insert nodeContext.node.id transitiveDepsSet artId_to_depIds
    in
    case Graph.checkAcyclic graph of
        Ok acyclicGraph ->
            { -- process the nodes in order of reverse topological sort, so that at the point when
              -- folding function processes node x, all its dependencies are already processed
              transitiveCounts =
                Graph.topologicalSort acyclicGraph
                    |> List.foldr (processNodeContext .outgoing) IntDict.empty
                    |> IntDict.map (\_ depSet -> Set.size depSet)

            -- process in topological order in order to get reverse deps
            , reverseTransitiveCounts =
                Graph.topologicalSort acyclicGraph
                    |> List.foldl (processNodeContext .incoming) IntDict.empty
                    |> IntDict.map (\_ depSet -> Set.size depSet)
            }

        Err edgesFormingCycle ->
            Debug.log
                ("The dependency graph was not acyclic! The following edges form a cycle: "
                    ++ toString edgesFormingCycle
                )
                { transitiveCounts = IntDict.empty
                , reverseTransitiveCounts = IntDict.empty
                }



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
