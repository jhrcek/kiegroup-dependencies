module Data.GAV
    exposing
        ( ArtifactId
        , GroupId
        , Version
        , artifactIdParser
        , artifactIdToString
        , groupIdParser
        , groupIdToString
        , versionParser
        , versionToString
        )

import UrlParser


type GroupId
    = GroupId


type ArtifactId
    = ArtifactId


type Version
    = Version


groupIdParser : UrlParser.Parser (GroupId -> a) a
groupIdParser =
    Debug.crash "TODO"


artifactIdParser : UrlParser.Parser (ArtifactId -> a) a
artifactIdParser =
    Debug.crash "TODO"


versionParser : UrlParser.Parser (Version -> a) a
versionParser =
    Debug.crash "TODO"


versionToString : Version -> String
versionToString =
    Debug.crash "TODO"


artifactIdToString : ArtifactId -> String
artifactIdToString =
    Debug.crash "TODO"


groupIdToString : GroupId -> String
groupIdToString =
    Debug.crash "TODO"
