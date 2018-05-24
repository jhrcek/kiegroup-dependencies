{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module Data.DepGraph where

import           Data.Aeson                        (ToJSON, object, toJSON,
                                                    (.=))
import           Data.Coordinate                   (Coordinate (..))
import qualified Data.Graph.Inductive              as Graph
import           Data.Graph.Inductive.Graph        (Context, LNode, Node, UEdge,
                                                    mkGraph)
import           Data.Graph.Inductive.PatriciaTree (Gr)
import qualified Data.Graph.Inductive.Query.DFS    as DFS
import           Data.IntMap                       (IntMap)
import qualified Data.IntMap                       as IntMap
import           Data.IntSet                       (IntSet)
import qualified Data.IntSet                       as IntSet
import qualified Data.List                         as List
import           Data.Maybe                        (listToMaybe)
import           Maven.DepGraph.Report             (Artifact (..),
                                                    Dependency (..),
                                                    Report (..))
import           Maven.DepGraph.Report.IO          (readReport)


constructDependencyGraphWithCounts :: FilePath -> IO DependencyGraphWithCounts
constructDependencyGraphWithCounts depGraphFile = do
    dependencyGraph <- constructDependencyGraph depGraphFile
    let depCounts = countDependencies dependencyGraph
        depGraphWithCounts = addCounts depCounts dependencyGraph
    return depGraphWithCounts

constructDependencyGraph :: FilePath -> IO DependencyGraph
constructDependencyGraph depGraphFile = either error fromReport <$> readReport depGraphFile


newtype DependencyGraph =
    DependencyGraph (Gr Coordinate ())
    deriving (Show)

fromReport :: Report -> DependencyGraph
fromReport report = DependencyGraph $ mkGraph nodes edges
  where
    nodes = toCoordinateNode <$> artifacts report
    edges = toDependencyEdge <$> dependencies report

toCoordinateNode :: Artifact -> LNode Coordinate
toCoordinateNode Artifact{numericId, groupId, artifactId, types, classifiers, version} =
   (numericId, coord)
  where
    coord = Coordinate
        { cGroupId = groupId
        , cArtifactId = artifactId
        , cPackaging = head types -- TODO make total
        , cQualifier = listToMaybe classifiers
        , cVersion = version
        }

toDependencyEdge :: Dependency -> UEdge
toDependencyEdge Dependency{numericFrom, numericTo} =
    (numericFrom, numericTo, ())




{- After merging all the dep trees, we add to each node additional info about
 number of direct / transitive / reverse direct / reverse transitive dependencies -}

data DependencyCounts = DependencyCounts
    { direct           :: IntMap Int
    , transitive       :: IntMap Int
    , revers           :: IntMap Int
    , reversTransitive :: IntMap Int
    } deriving Show

countDependencies :: DependencyGraph -> DependencyCounts
countDependencies (DependencyGraph graph) = DependencyCounts{..}
  where
    direct = IntMap.fromList $ fmap (\nodeId -> (nodeId, countDirectOutgoing nodeId)) topSortedNodeIds
    revers = IntMap.fromList $ fmap (\nodeId -> (nodeId, countDirectIncoming nodeId)) topSortedNodeIds

    transitive       = IntSet.size <$> foldr (processNodeContext Graph.suc') IntMap.empty topSortedContexts
    reversTransitive = IntSet.size <$> foldr (processNodeContext Graph.pre') IntMap.empty (List.reverse topSortedContexts)

    topSortedNodeIds :: [Node]
    topSortedNodeIds = DFS.topsort graph

    topSortedContexts :: [Context Coordinate ()]
    topSortedContexts = Graph.context graph <$> topSortedNodeIds

    processNodeContext :: (Context Coordinate () -> [Graph.Node]) -> Context Coordinate () -> IntMap IntSet -> IntMap IntSet
    processNodeContext  direction ctx artId_to_depIds =
        IntMap.insert (Graph.node' ctx) transitiveDepsSet artId_to_depIds
      where
        directDepIds = direction ctx

        transitiveDepsSet = List.foldl'
            (\acc depId -> IntSet.union acc (IntMap.findWithDefault IntSet.empty depId artId_to_depIds))
            (IntSet.fromList directDepIds)
            directDepIds

    {- These 2 functions work around the fact that dependency edge A -> B can appear with different scope in different dependency trees
     That leads to Graph.outdeg / Graph.indeg return larger numbers than the actual number of direct dependencies,
     because A -> B is then counted multiple times (once for each different scope)
    -}
    countDirectOutgoing = IntSet.size . IntSet.fromList . Graph.suc graph
    countDirectIncoming = IntSet.size . IntSet.fromList . Graph.pre graph

-- Final representation to be sent to frontend, which - in addition to coordinate info for each artifact -
-- contains info about number of its dependencies
newtype DependencyGraphWithCounts =
    DependencyGraphWithCounts (Gr (Coordinate, (Int, Int, Int, Int)) ())
    deriving (Show)

instance ToJSON DependencyGraphWithCounts where
    toJSON (DependencyGraphWithCounts graph) = object
        [ "nodes" .= nodes
        , "edges" .= edges
        ]
      where
        nodes = Graph.labNodes graph
        edges = Graph.edges graph

addCounts :: DependencyCounts -> DependencyGraph -> DependencyGraphWithCounts
addCounts counts (DependencyGraph graph) = DependencyGraphWithCounts $
    Graph.gmap
        (\(inadj, nodeId,  coordinate                              , outadj) ->
          (inadj, nodeId, (coordinate, lookupCounts nodeId counts) , outadj))
        graph

lookupCounts :: Graph.Node -> DependencyCounts -> (Int, Int, Int, Int)
lookupCounts nodeId counts = (revTrans, rev, dir, trans)
  where
    lkp :: (DependencyCounts -> IntMap Int) -> Int
    lkp mapGetter = IntMap.findWithDefault 0 nodeId (mapGetter counts)
    dir      = lkp direct
    trans    = lkp transitive
    rev      = lkp revers
    revTrans = lkp reversTransitive
