#!/usr/bin/env stack
-- stack --install-ghc runghc
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
import Data.Either
import qualified Data.Text as Txt
import qualified Data.Text.IO as Txt
import Filesystem.Path.CurrentOS as OSPath
import Prelude hiding (FilePath)
import qualified TGF
import Turtle
import qualified Data.IntMap.Strict as IntMap
import Data.List


main :: IO ()
main = do
    putStrLn "Parsing dependency tree files"
    reportFilepaths <- fold (ls "dependency-trees") Foldl.list
    parsedDepTrees <- rights <$> mapM loadDepTree reportFilepaths
    putStrLn $ "\nDONE, " <> show (length parsedDepTrees) <> " files successfuly parsed"

    -- TODO consolidate all deps into single file
    putStrLn "Calculating unique dependencies"
    let uniqueDeps = nub . sort $ concatMap (IntMap.elems . TGF.depNames) parsedDepTrees
    putStrLn $ "Found " <> show (length uniqueDeps) <> " unique dependencies"

    putStrLn "Calculating dependencies just by Grou / Artifact equality"
    let depsGroupAndArtifact = nubBy TGF.groupArtifactEquality . sort $ concatMap (IntMap.elems . TGF.depNames) parsedDepTrees
    putStrLn $ "Found " <> show (length depsGroupAndArtifact) <> " dependencies with equal GroupId / ArtifactId"


loadDepTree :: FilePath -> IO (Either String TGF.Deps)
loadDepTree tgfFile = do
    tgfContents <- Txt.readFile (filepathToString tgfFile)
    putStr "."
    case TGF.parseDeps tgfContents of
      Right deps -> return $ Right deps
      Left er -> do
           putStrLn $ "\nWARNING: failed to parse " ++ filepathToString tgfFile ++ ", error was " ++ er
           return $ Left er

-- TODO deduplicate
filepathToString :: FilePath -> String
filepathToString = Txt.unpack . either (error . show) id . OSPath.toText
