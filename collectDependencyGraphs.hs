#!/usr/bin/env stack
-- stack --install-ghc runghc
  --package turtle
  --package system-filepath
  --package hxt
  --package text

{- Collect dependency trees of each maven module
PREREQUISITE:  You must run the following to generate dependency tree reports for each module
$ cd droolsjbpm-build-bootstrap/scripts
$ ./mvn-all.sh dependency:tree -DoutputType=tgf -DoutputFile=deps.tgf

This script will rename each of those files to the form "<groupId>_<artifactId>.tgf"
and will copy them to "dependency-trees"
-}
{-# LANGUAGE OverloadedStrings #-}
import Data.Text (Text)
import qualified Data.Text as Txt
import qualified Filesystem.Path as Filepath
import qualified Filesystem.Path.CurrentOS as OSPath
import Prelude hiding (FilePath)
import Text.XML.HXT.Core (getText, hasName, readDocument, runX, (//>), (/>))
import Turtle
import qualified Turtle.Pattern as Pattern

depTreesDir :: FilePath
depTreesDir = "dependency-trees"

main :: IO ()
main = do
  testdir depTreesDir >>= (\exists -> when exists $ rmtree depTreesDir)
  mkdir depTreesDir
  putStrLn "Resursively searching for deps.tgr"
  sh $ findDependencyReports >>= copyToTarget
  putStrLn "\nFiles copied to dependency-trees"

findDependencyReports :: Shell FilePath
findDependencyReports = pwd >>= Turtle.find (Pattern.suffix "/deps.tgf")

toTargetFileName :: FilePath -> IO FilePath
toTargetFileName sourceReport = do
    let associatedPom = Filepath.directory sourceReport </> "pom.xml"
    pomExists <- testfile associatedPom
    unless pomExists . error $ "I was expecting pom.xml in the same dir as " <> show sourceReport <> " but it doesn't exist!"
    (GroupId gid, ArtifactId aid) <- getGroupAndArtifactFromPom associatedPom
    return $ depTreesDir </> (OSPath.fromText $ gid <> "_" <> aid) <.> "tgf"


copyToTarget :: FilePath -> Shell ()
copyToTarget sourceReport = liftIO $ do
    targetReport <- liftIO $ toTargetFileName sourceReport
    targetAlreadyExists <- testfile targetReport
    if targetAlreadyExists
      then putStrLn $ "WARNING: " <> show targetReport <> " already exists - NOT overwriting!"
      else cp sourceReport targetReport

getGroupAndArtifactFromPom :: FilePath -> IO (GroupId, ArtifactId)
getGroupAndArtifactFromPom pomFile = do
    let doc = readDocument [] (filepathToString pomFile)
    [groupId] <- (runX $ doc /> groupIdArr) >>= (\gidList -> if null gidList then (runX $ doc /> parentGroupIdArr) else return gidList)
    [artifactId] <- runX $ doc /> artifactIdArr
    putStr "." -- progress indicator
    return (GroupId $ Txt.pack groupId, ArtifactId $ Txt.pack artifactId)
  where
    groupIdArr = hasName "project" /> hasName "groupId" //> getText
    parentGroupIdArr = hasName "project" /> hasName "parent" /> hasName "groupId" //> getText
    artifactIdArr = hasName "project" /> hasName "artifactId" //> getText

newtype GroupId = GroupId Text deriving Show
newtype ArtifactId = ArtifactId Text deriving Show

filepathToString :: FilePath -> String
filepathToString = Txt.unpack . either (error . show) id . OSPath.toText
