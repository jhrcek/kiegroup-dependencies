{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
module DepGraph
    ( DependencyGraphWithCounts
    , constructDependencyGraphWithCounts
    ) where
import qualified Debug.Trace as D
import           Control.Foldl                     (FoldM (FoldM))
import           Control.Monad.State
import           Data.Aeson                        (ToJSON, object, toJSON,
                                                    (.=))
import qualified Data.Graph.Inductive              as Graph
import           Data.Graph.Inductive.Graph        (Context, Node)
import           Data.Graph.Inductive.PatriciaTree (Gr)
import qualified Data.Graph.Inductive.Query.DFS    as DFS
import           Data.IntMap                       (IntMap, (!))
import qualified Data.IntMap                       as IntMap
import           Data.IntSet                       (IntSet)
import qualified Data.IntSet                       as IntSet
import qualified Data.List                         as List
import           Data.Map                          (Map)
import qualified Data.Map                          as Map
import           Data.Monoid                       ((<>))
import           Data.Tuple                        (swap)
import           Prelude                           hiding (FilePath)
import           TGF                               (Coordinate, Scope,
                                                    TgfDepGraph)
import qualified TGF
import qualified TGF.IO
import           Turtle                            (FilePath, Shell)
import qualified Turtle
import qualified Turtle.Pattern                    as Pattern

constructDependencyGraphWithCounts :: FilePath -> IO DependencyGraphWithCounts
constructDependencyGraphWithCounts kiegroupDir = do
    dependencyGraph <- constructDependencyGraph kiegroupDir
    let depCounts = countDependencies dependencyGraph
        depGraphWithCounts = addCounts depCounts dependencyGraph
    return depGraphWithCounts

{- Traverse kiegroup dir recursively looking for deps.tgf files.
   Parse each file and add dependency info to global dependency graph
 -}
constructDependencyGraph :: FilePath -> IO DependencyGraph
constructDependencyGraph kiegroupDir =
    toGraph <$> collectDeptTrees kiegroupDir

collectDeptTrees :: FilePath -> IO DepGraphAcc
collectDeptTrees kiegroupDir =
    Turtle.foldIO (findDependencyReports kiegroupDir) foldDepTrees

findDependencyReports :: FilePath -> Shell FilePath
findDependencyReports =
    Turtle.find (Pattern.suffix "/deps.tgf")


-- Serves as an intermediate accumulator to merge all dependency trees fro individual deps.tgf files
-- After merging of each dep. tree it's converted to DependencyGraph
data DepGraphAcc = DepGraphAcc
    { coordinates :: Map Coordinate Int
    , directDeps  :: Map (Int, Int) [(Scope, Int)]
    } deriving Show

foldDepTrees :: FoldM IO FilePath DepGraphAcc
foldDepTrees =
    FoldM step initial extract
  where

    step :: DepGraphAcc -> FilePath -> IO DepGraphAcc
    step depTrees depsFile = do
        eitherErrTgf <- TGF.IO.loadDependencyTree depsFile
        let tgf = either error id eitherErrTgf
        return $ processTgf depTrees tgf

    initial :: IO DepGraphAcc
    initial = return DepGraphAcc
        { coordinates = Map.empty
        , directDeps = Map.empty
        }

    extract :: DepGraphAcc -> IO DepGraphAcc
    extract = return


processTgf :: DepGraphAcc -> TgfDepGraph -> DepGraphAcc
processTgf depTrees tgf = DepGraphAcc
    { coordinates = newCoordinates
    , directDeps = newDirectDeps
    }
  where
    oldArtifacts = coordinates depTrees
    oldDirectDeps = directDeps depTrees

    nodeDecls = TGF.nodeDeclarations tgf
    edgeDecls = TGF.edgeDeclarations tgf

    -- Accumulator is pair of
    --      ( global map of all known artifact coordinates to their IDs
    --      , map of coord IDs local to processed TGF file to global IDs)
    (newCoordinates, oldToNewIdMap) = List.foldl' addNode (oldArtifacts, IntMap.empty) nodeDecls

    directDepsFromTgf = Map.fromListWith (<>) $ map (\(from, to, label) -> ( (oldToNewIdMap ! from, oldToNewIdMap ! to) ,  [(label, 1)])) edgeDecls

    newDirectDeps = Map.unionWith (<>) directDepsFromTgf oldDirectDeps

    addNode :: (Map Coordinate Int, IntMap Int) -> (Graph.Node, Coordinate) -> (Map Coordinate Int, IntMap Int)
    addNode (arts, m) (tgfNodeId, coord) =
      let (arts', coordId) = case Map.lookup coord arts of
              Nothing  -> let newCid = Map.size arts in (Map.insert coord newCid arts, newCid)
              Just cid -> (arts, cid)
          m' = IntMap.insert tgfNodeId coordId m
      in (arts', m')

-- Global dependency graph into which all dependency trees were merged
newtype DependencyGraph =
    DependencyGraph (Gr Coordinate [(Scope, Int)])
    deriving (Show)

toGraph :: DepGraphAcc -> DependencyGraph
toGraph DepGraphAcc{coordinates, directDeps} =
    DependencyGraph $ Graph.mkGraph nodes edges
  where
    nodes = swap <$> Map.assocs coordinates
    edges = (\((from,to), scopeCounts) -> (from, to,
      let cnts = Map.toList $ Map.fromListWith (+) scopeCounts
      in if length cnts > 1 then
            D.trace (show cnts) cnts
         else
           cnts)) <$> Map.toList directDeps

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

    topSortedContexts :: [Context Coordinate [(Scope, Int)]]
    topSortedContexts = Graph.context graph <$> topSortedNodeIds

    processNodeContext :: (Context Coordinate [(Scope, Int)] -> [Graph.Node]) -> Context Coordinate [(Scope, Int)] -> IntMap IntSet -> IntMap IntSet
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
    DependencyGraphWithCounts (Gr (Coordinate, (Int, Int, Int, Int)) [(Scope, Int)])
    deriving (Show)

instance ToJSON DependencyGraphWithCounts where
    toJSON (DependencyGraphWithCounts graph) = object
        [ "nodes" .= nodes
        , "edges" .= edges
        ]
      where
        nodes = Graph.labNodes graph
        edges = Graph.labEdges graph

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
