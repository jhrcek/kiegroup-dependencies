{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Control.Foldl                     (FoldM (FoldM))
import           Control.Monad.State
import           Data.Aeson                        (ToJSON, encode, object,
                                                    toJSON, (.=))
import qualified Data.ByteString.Lazy              as BS
import qualified Data.Graph.Inductive              as Graph
import           Data.Graph.Inductive.PatriciaTree (Gr)
import           Data.IntMap                       (IntMap, (!))
import qualified Data.IntMap                       as IntMap
import           Data.List                         (foldl')
import           Data.Map                          (Map)
import qualified Data.Map                          as Map
import           Data.Set                          (Set)
import qualified Data.Set                          as Set
import           Data.Tuple                        (swap)
import           Prelude                           hiding (FilePath)
import           TGF                               (Coordinate, Scope,
                                                    TgfDepGraph)
import qualified TGF
import qualified TGF.IO
import           Turtle                            (FilePath, Shell, argPath,
                                                    options)
import qualified Turtle
import qualified Turtle.Pattern                    as Pattern

{-| USAGE
1) Generate dependency report file(s) for one or more maven projects
$ mvn dependency:tree -DoutputType=tgf -DoutputFile=deps.tgf

2) Run this tool to collect the data, providing path to directory where all maven projects are located as CLI arg
$ stack exec collect-deps -- PATH/TO/COMMON/DIR
-}
main :: IO ()
main = do
    kiegroupDir <- parseArgs
    dependencyGraph <- constructDependencyGraph kiegroupDir
    BS.writeFile outputFile $ encode dependencyGraph
    putStrLn $ "Dependency graph written to " ++ outputFile

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

parseArgs :: MonadIO io => io FilePath
parseArgs =
    options "Dependency collector" $ argPath "KIEGROUP_DIR" "Directory containing all kiegroup repos"

-- Serves as an intermediate accumulator to merge all dependency trees fro individual deps.tgf files
-- After merging of each dep. tree it's converted to DependencyGraph
data DepGraphAcc = DepGraphAcc
    { coordinates :: Map Coordinate Int
    , directDeps  :: Set (Int, Int, Scope)
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
        , directDeps = Set.empty
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
    (newCoordinates, oldToNewIdMap) = foldl' addNode (oldArtifacts, IntMap.empty) nodeDecls

    directDepsFromTgf = Set.fromList $ map (\(from, to, label) -> (oldToNewIdMap ! from, oldToNewIdMap ! to, label)) edgeDecls

    newDirectDeps = Set.union directDepsFromTgf oldDirectDeps

    addNode :: (Map Coordinate Int, IntMap Int) -> (Graph.Node, Coordinate) -> (Map Coordinate Int, IntMap Int)
    addNode (arts, m) (tgfNodeId, coord) =
      let (arts', coordId) = case Map.lookup coord arts of
              Nothing  -> let newCid = Map.size arts in (Map.insert coord newCid arts, newCid)
              Just cid -> (arts, cid)
          m' = IntMap.insert tgfNodeId coordId m
      in (arts', m')

newtype DependencyGraph =
    DependencyGraph (Gr Coordinate Scope)
    deriving (Show)

toGraph :: DepGraphAcc -> DependencyGraph
toGraph DepGraphAcc{coordinates, directDeps} =
    DependencyGraph $ Graph.mkGraph nodes edges
  where
    nodes = swap <$> Map.assocs coordinates
    edges = Set.toList directDeps

instance ToJSON DependencyGraph where
    toJSON (DependencyGraph graph) = object
        [ "nodes" .= nodes
        , "edges" .= edges
        ]
      where
        nodes = Graph.labNodes graph
        edges = Graph.labEdges graph

outputFile :: String
outputFile = "dependency-graph.json"
