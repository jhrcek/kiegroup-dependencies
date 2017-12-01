#!/usr/bin/env stack
-- stack --install-ghc runghc
  --package turtle
  --package system-filepath
  --package text

{-
This script will copy all of those deps.tgf files generated by mvn dependency:tree
to "dependency-trees" directory and will rename it to the form "<groupId>_<artifactId>_<packaging>.tgf"

USAGE
$ cd droolsjbpm-build-bootstrap/scripts
$ ./mvn-all.sh dependency:tree -DoutputType=tgf -DoutputFile=deps.tgf
$ cd ../.. # go back to kiegroup folder containing all the projects
$ ./collectDependencyGraphs.hs
-}

{-# LANGUAGE OverloadedStrings #-}
import Data.Text (Text)
import qualified Data.Text as Txt
import qualified Data.Text.IO as Txt
import qualified Filesystem.Path.CurrentOS as OSPath
import Prelude hiding (FilePath)
import Turtle
import qualified Turtle.Pattern as Pattern

main :: IO ()
main = do
  testdir depTreesDir >>= (\exists -> when exists $ rmtree depTreesDir)
  mkdir depTreesDir
  putStrLn "Searching for deps.tgf"
  sh $ findDependencyReports >>= copyToTarget
  putStrLn "\nFiles copied to dependency-trees"

depTreesDir :: FilePath
depTreesDir = "dependency-trees"


findDependencyReports :: Shell FilePath
findDependencyReports = pwd >>= Turtle.find (Pattern.suffix "/deps.tgf")


{- We want to move the output file of dependency:analyze, like "drools-wb/drools-wb-webapp/deps.tgf"
   to a single folder where each file will have the name of the form <groupId>_<artifactId>.tgf
-}
toTargetFileName :: FilePath -> IO FilePath
toTargetFileName sourceReport = do
    reportContents <- Txt.readFile . Txt.unpack $ filepathToText sourceReport
    case Txt.lines reportContents of
      (firstLine:_) -> case Txt.words firstLine of
          (_:gav:_) -> case Txt.splitOn ":" gav of
              (groupId:artifactId:packaging:_) -> return $ depTreesDir </> (OSPath.fromText $ groupId <> "_" <> artifactId <> "_" <> packaging) <.> "tgf"
              _                                -> die $ "Unexpected format of GAV '" <> gav <> "' in " <> filepathToText sourceReport
          _ -> die $ "Unexpected format of first line in " <> filepathToText sourceReport
      _ -> die $ "File doesn't have lines " <> filepathToText sourceReport


copyToTarget :: FilePath -> Shell ()
copyToTarget sourceReport = liftIO $ do
    targetReport <- liftIO $ toTargetFileName sourceReport
    targetAlreadyExists <- testfile targetReport
    if targetAlreadyExists
      then putStrLn $ "WARNING: " <> show targetReport <> " already exists - NOT overwriting!"
      else putStr "." {- progress indicator -} >> cp sourceReport targetReport


filepathToText :: FilePath -> Text
filepathToText = either (error . show) id . OSPath.toText