module Data.DependencyTree
    exposing
        ( Tree(Node)
        , buildDirectDependencyTree
        , buildTransitiveDependencyTree
        )

import Data.DependencyGraph exposing (DependencyContext, DependencyGraph, DependencyNode)
import Data.Scope exposing (Scope)
import Graph exposing (Adjacency, Node, NodeId)
import Graph.Tree as Tree
import IntDict


type Tree a
    = Node a (List (Tree a))


{-| Take Tree produced by Graph.dfsTree and convert it to
rose tree which -for all non-root nodes - contains scope

We're also passing the full graph as a workaround for
<https://github.com/elm-community/graph/issues/18>

-}
buildTransitiveDependencyTree : DependencyGraph -> Tree.Tree DependencyContext -> Tree ( DependencyNode, Maybe Scope )
buildTransitiveDependencyTree graph tree =
    buildTransitiveHelp graph Nothing tree


buildTransitiveHelp : DependencyGraph -> Maybe NodeId -> Tree.Tree DependencyContext -> Tree ( DependencyNode, Maybe Scope )
buildTransitiveHelp graph mParentId t =
    case Tree.root t of
        Nothing ->
            Node ( dummy, Nothing ) []

        Just ( ctx, children ) ->
            let
                maybeScope =
                    mParentId
                        |> Maybe.andThen (\parentId -> Graph.get parentId graph)
                        |> Maybe.andThen (\parentCtx -> IntDict.get ctx.node.id parentCtx.outgoing)
            in
            Node
                ( ctx.node, maybeScope )
                (List.map (buildTransitiveHelp graph (Just ctx.node.id)) children)


buildDirectDependencyTree : DependencyGraph -> DependencyNode -> Adjacency Scope -> Tree ( DependencyNode, Maybe Scope )
buildDirectDependencyTree graph rootNode adjacency =
    let
        children =
            IntDict.toList adjacency
                |> List.map
                    (\( childrenNodeId, scope ) ->
                        ( Graph.get childrenNodeId graph
                            |> Maybe.map .node
                            |> Maybe.withDefault dummy
                        , Just scope
                        )
                    )
                -- Sort children by scope
                |> List.sortBy (toString << Tuple.second)
                |> List.map (\nodeAndScope -> Node nodeAndScope [])
    in
    Node ( rootNode, Nothing ) children


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
        , directDepsCount = 0
        , transitiveDepsCount = 0
        , reverseDirectDepsCount = 0
        , reverseTransitiveDepsCount = 0
        }
    }
