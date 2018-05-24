{-# LANGUAGE OverloadedStrings #-}

module Maven.DepGraph.Report where
import           Data.Aeson      (FromJSON, withObject, (.!=), (.:), (.:?))
import qualified Data.Aeson      as Aeson
import           Data.Coordinate (Scope)
import           Data.Text       (Text)

{- Modelling the json output of depgraph-maven-plugin as produced by

mvn clean com.github.ferstl:depgraph-maven-plugin:3.1.0:aggregate -DgraphFormat=json
-}

data Report = Report
    { graphName    :: Text
    , artifacts    :: [Artifact]
    , dependencies :: [Dependency]
    } deriving Show

instance FromJSON Report where
    parseJSON = withObject "dependency graph" $ \v -> Report
        <$> v .: "graphName"
        <*> v .: "artifacts"
        <*> v .: "dependencies"

data Artifact = Artifact
    { id          :: Text
    , numericId   :: Int
    , groupId     :: Text
    , artifactId  :: Text
    , version     :: Text
    , classifiers :: [Text] -- represents qualifiers
    , scopes      :: [Scope]
    , types       :: [Text] --represents packagings
    } deriving Show

instance FromJSON Artifact where
    parseJSON = withObject "artifact" $ \v -> Artifact
        <$> v .: "id"
        <*> v .: "numericId"
        <*> v .: "groupId"
        <*> v .: "artifactId"
        <*> v .: "version"
        <*> v .:? "classifiers" .!= []
        <*> v .: "scopes"
        <*> v .: "types"

data Dependency = Dependency
    { from        :: Text
    , to          :: Text
    , numericFrom :: Int
    , numericTo   :: Int
    , resolution  :: Text
    } deriving Show

instance FromJSON Dependency where
    parseJSON = withObject "dependency" $ \v -> Dependency
        <$> v .: "from"
        <*> v .: "to"
        <*> v .: "numericFrom"
        <*> v .: "numericTo"
        <*> v .: "resolution"
