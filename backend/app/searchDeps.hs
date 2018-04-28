#!/usr/bin/env stack
-- stack script --resolver=lts-11.6 --package turtle,text,foldl,directory

{- Find which pom.xml files declare jars from kie-wb.war/WEB-INF/lib as dependency

How to use this script?
1. Make a list of jar files
$ cd kie-wb.war/WEB-INF/lib
$ ls -1 > PATH/TO/kiegroup/jars

2. Put this script in "kiegroup" folder with all github projects cloned
$ cd PATH/TO/kiegroup
$ ./searchDeps.hs

OUTPUT
Report about which poms are using which artifacts
with warnings when artifact is candidate for deletion (used in too few poms)
-}

{-# LANGUAGE OverloadedStrings #-}
import qualified Control.Foldl as Fold
import Control.Monad (forM_)
import Data.Char (isDigit)
import Data.Text (Text)
import qualified Data.Text as Txt
import qualified Data.Text.IO as Txt
import System.Directory (doesFileExist)
import Turtle

newtype JarFileName = JarFileName Text
newtype ArtifactId = ArtifactId Text

main :: IO ()
main = do
  jarFileNames <- readJarFileNames
  let artifactIds = fmap toArtifactId jarFileNames
  forM_ artifactIds reportUsage

reportUsage :: ArtifactId -> IO ()
reportUsage (ArtifactId aid) = do
    pomsWithOccurence <- fmap lineToText <$> fold (grepArtifactUsageInPoms (ArtifactId aid)) Fold.list :: IO [Text]
    let numberOfPoms = length pomsWithOccurence
    if numberOfPoms < 2
      then do
        Txt.putStrLn $ "========== DELETION CANDIDATE : " <> aid <> " is used in " <> Txt.pack (show numberOfPoms) <> " poms:"
        mapM_ (Txt.putStrLn . ("    "<>) ) pomsWithOccurence
      else
        Txt.putStrLn $ aid <> " is used in " <> Txt.pack (show numberOfPoms) <> " poms"

readJarFileNames :: IO [JarFileName]
readJarFileNames = do
  exists <- doesFileExist "jars"
  unless exists (die "Please provide list of jar filenames from WEB-INF/lib, one file per line, in a file called 'jars'")
  (fmap JarFileName . Txt.lines) <$> Txt.readFile "jars"

grepArtifactUsageInPoms :: ArtifactId -> Shell Line
grepArtifactUsageInPoms (ArtifactId artifactId) =
    -- Why "|| true"? When grep finds no ocurrence, it returns 1, leading to Shell throwing ExitCode exception -> just return empty list of lines in that case
    inshell ("grep --recursive --files-with-matches --include=pom.xml '<artifactId>" <> artifactId <> "</artifactId>' || true") empty

toArtifactId :: JarFileName -> ArtifactId
toArtifactId (JarFileName jfn) =
    ArtifactId . Txt.intercalate "-" . takeWhile (not . startsWithNumber) $ Txt.splitOn "-" jfn

startsWithNumber :: Text -> Bool
startsWithNumber = maybe False (isDigit . fst) . Txt.uncons
