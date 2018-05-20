{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Control.Monad.IO.Class (MonadIO)
import qualified Data.Aeson             as Aeson
import qualified Data.ByteString.Lazy   as BS
import qualified DepGraph
import           Prelude                hiding (FilePath)
import           Turtle                 (FilePath, argPath, options)

{-| USAGE
1) Generate dependency report file(s) for one or more maven projects
$ mvn dependency:tree -DoutputType=tgf -DoutputFile=deps.tgf

2) Run this tool to collect the data, providing path to directory where all maven projects are located as CLI arg
$ stack exec collect-deps -- PATH/TO/COMMON/DIR
-}
main :: IO ()
main = do
    kiegroupDir <- parseArgs
    dependencyGraph <- DepGraph.constructDependencyGraphWithCounts kiegroupDir
    BS.writeFile outputFile $ Aeson.encode dependencyGraph
    putStrLn $ "Dependency graph written to " ++ outputFile

parseArgs :: MonadIO io => io FilePath
parseArgs =
    options "Dependency collector" $ argPath "KIEGROUP_DIR" "Directory containing all kiegroup repos"

outputFile :: String
outputFile = "dependency-graph.json"
