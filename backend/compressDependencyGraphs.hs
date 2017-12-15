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
import Data.List
import qualified Data.Text as Txt
import qualified Data.Text.IO as Txt
import Filesystem.Path.CurrentOS as OSPath
import Prelude hiding (FilePath)
import qualified TGF
import Turtle
import Util (filepathToString, filepathToText)


main :: IO ()
main = do
    putStrLn "Parsing dependency tree files"
    reportFilepaths <- getTgfReports
    parsedDepTrees <- rights <$> mapM loadDepTree reportFilepaths
    putStrLn $ "\nDONE, " <> show (length parsedDepTrees) <> " files successfuly parsed"

    let allDepsFromAllTrees = sort $ concatMap (IntMap.elems . TGF.depNames) parsedDepTrees

    printDepsSummary allDepsFromAllTrees

    putStrLn "Generating index.html"
    generateIndexHtml


printDepsSummary :: [TGF.Dependency] -> IO ()
printDepsSummary allDepsFromAllTrees =
    let uniqueDeps = nub allDepsFromAllTrees
        uniqueCoords = nub $ map TGF.dCoordinate uniqueDeps
        uniqueDepsByGroupAndArtifactId = nubBy TGF.equalByGroupAndArtifact allDepsFromAllTrees
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


{-| Genereates file index.html which references elm.js to drive the app
    and provides the list of artifacts as flags for elm
 -}
generateIndexHtml :: IO ()
generateIndexHtml = do
    depLines <- fmap tgfFilenameToDepLine <$> getTgfReports
    let flagsForElm = Txt.cons '[' . (`Txt.snoc` ']') $ Txt.intercalate "," depLines
    Txt.writeFile "dependency-trees/index.html" $ Txt.unlines
        ["<!DOCTYPE HTML>"
        ,"<html>"
        ,"<head>"
        ,"  <title>kiegroup POMs cleanup</title>"
        ,"  <script src=\"elm.js\"></script>"
        ,"</head>"
        ,"<body>"
        ,"  <script type=\"text/javascript\">"
        ,"    Elm.Main.fullscreen(",flagsForElm,");"
        ,"  </script>"
        ,"</body>"
        ,"</html>"]
  where
    tgfFilenameToDepLine = (`Txt.snoc` '"') . Txt.cons '"' .  filepathToText . dropExtension . filename


getTgfReports :: IO [FilePath]
getTgfReports = fold (Turtle.find (suffix  ".tgf") "dependency-trees") Foldl.list
