{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Control.Foldl       (FoldM (FoldM))
import           Control.Monad.State
import           Data.IntMap         (IntMap, (!))
import qualified Data.IntMap         as IntMap
import           Data.List           (foldl')
import           Data.Map            (Map)
import qualified Data.Map            as Map
import           Data.Set            (Set)
import qualified Data.Set            as Set
import           Data.Text           (Text)
import           Data.Tuple          (swap)
import           Prelude             hiding (FilePath)
import           TGF                 (Coordinate, NodeId, TGF)
import qualified TGF
import qualified TGF.IO
import           Turtle              (FilePath, Shell, argPath, options)
import qualified Turtle
import qualified Turtle.Pattern      as Pattern

main :: IO ()
main = do
    kiegroupDir <- parseArgs
    depTrees <- collectDeptTrees kiegroupDir
    print depTrees

    let cidToCoord = flipMap $ artifacts depTrees
    print cidToCoord

{- Traverse kiegroup dir recursively looking for deps.tgf files.
   Parse each file and add dependency info
 -}
collectDeptTrees :: FilePath -> IO DepTrees
collectDeptTrees kiegroupDir = Turtle.foldIO (findDependencyReports kiegroupDir) foldDepTrees


findDependencyReports :: FilePath -> Shell FilePath
findDependencyReports = Turtle.find (Pattern.suffix "/deps.tgf")

parseArgs :: MonadIO io => io FilePath
parseArgs =
    options "Dependency collector" $ argPath "KIEGROUP_DIR" "Directory containing all kiegroup repos"


data DepTrees = DepTrees
    { artifacts  :: Map Coordinate Int
    , directDeps :: Set (Int, Int)
    } deriving Show

foldDepTrees :: FoldM IO FilePath DepTrees
foldDepTrees = FoldM step initial extract
  where

    step :: DepTrees -> FilePath -> IO DepTrees
    step depTrees depsFile = do
        eitherErrTgf <- TGF.IO.loadTgfFromFile depsFile
        let tgf = either error id eitherErrTgf
        return $ processTgf depTrees tgf

    initial :: IO DepTrees
    initial = return DepTrees
        { artifacts = Map.empty
        , directDeps = Set.empty
        }

    extract :: DepTrees -> IO DepTrees
    extract = return


processTgf :: DepTrees -> TGF -> DepTrees
processTgf depTrees tgf = DepTrees
    { artifacts = newArtifacts
    , directDeps = newDirectDeps
    }
  where
    oldArtifacts = artifacts depTrees
    oldDirectDeps = directDeps depTrees

    nodeDecls = TGF.nodeDeclarations tgf
    edgeDecls = TGF.edgeDeclarations tgf

    -- Insert artifacts which we haven't seen
    -- and accumulate map that maps from NodeIds used in the TGF file to Coordinate IDs
    (newArtifacts, oldToNewIdMap) = foldl' fun (oldArtifacts, IntMap.empty) nodeDecls

    directDepsFromTgf = Set.fromList $ map (\(from, to, _label) -> (oldToNewIdMap ! from, oldToNewIdMap ! to)) edgeDecls

    newDirectDeps = Set.union directDepsFromTgf oldDirectDeps

    fun :: (Map Coordinate Int, IntMap Int) -> (NodeId, Text) -> (Map Coordinate Int, IntMap Int)
    fun (arts, m) (tgfNodeId, txt) =
      let coord = either error id $ TGF.readCoordinate txt
          (arts', coordId) = case Map.lookup coord arts of
              Nothing  -> let newCid = Map.size arts in (Map.insert coord newCid arts, newCid)
              Just cid -> (arts, cid)
          m' = IntMap.insert tgfNodeId coordId m
      in (arts', m')

flipMap :: Map Coordinate Int -> IntMap Coordinate
flipMap = IntMap.fromList . fmap swap .  Map.assocs
