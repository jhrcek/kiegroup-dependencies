#!/usr/bin/env stack
-- stack runghc
  --package turtle
  --package system-filepath
  --package text
  --package foldl

{-
This script processes output of collectDependencyGraphs.hs

by analyzing all tfg files from dependency-trees folder
TODO
-}

{-# LANGUAGE OverloadedStrings #-}

import qualified Control.Foldl as Foldl
import Control.Monad
import Data.Either
import qualified Data.IntMap.Strict as IntMap
import qualified Data.List as List
import qualified Data.Text.IO as Txt
import Filesystem.Path.CurrentOS as OSPath
import Prelude hiding (FilePath)
import qualified TGF
import Turtle
import Util (filepathToString)


main :: IO ()
main = do
    putStrLn "Parsing dependency tree files"
    reportFilepaths <- getTgfReports
    parsedDepTrees <- rights <$> mapM loadDepTree reportFilepaths
    putStrLn $ "\nDONE, " <> show (length parsedDepTrees) <> " files successfuly parsed"
    let ns = map (length . snd) . IntMap.toList . IntMap.fromListWith (++) . fmap (fmap (:[])) $ concatMap (IntMap.toList . TGF.depNames) parsedDepTrees
    print ns
    -- let allDepsFromAllTrees = sort $ concatMap (IntMap.elems . TGF.depNames) parsedDepTrees
    --     uniqueDeps = nub allDepsFromAllTrees
    -- mapM_ print . sort . map ( \g -> (length g, head g)) .group . sort $ map (TGF.cPackaging . TGF.dCoordinate) uniqueDeps
    --printDepsSummary allDepsFromAllTrees


printDepsSummary :: [TGF.Dependency] -> IO ()
printDepsSummary allDepsFromAllTrees =
    let uniqueDeps = List.nub allDepsFromAllTrees
        uniqueCoords = List.nub $ map TGF.dCoordinate uniqueDeps
        uniqueDepsByGroupAndArtifactId = List.nubBy TGF.equalByGroupAndArtifact allDepsFromAllTrees
    in do
        putStr "Calculating unique dependencies ... "
        print $ length uniqueDeps
        putStr "Calculating unique coordinates  ... "
        print $ length uniqueCoords
        putStr "Calculating unique dependencies by just GroupId + ArtifactId equality ... "
        print $ length uniqueDepsByGroupAndArtifactId


loadDepTree :: FilePath -> IO (Either String TGF.Deps)
loadDepTree tgfFile = do
    tgfContents <- Txt.readFile (filepathToString tgfFile)
    putStr "."
    case TGF.parseDeps tgfContents of
      Right deps -> return $ Right deps
      Left er -> do
           putStrLn $ "\nWARNING: failed to parse " ++ filepathToString tgfFile ++ ", error was " ++ er
           return $ Left er


getTgfReports :: IO [FilePath]
getTgfReports = fold (Turtle.find (suffix  ".tgf") ".") Foldl.list
