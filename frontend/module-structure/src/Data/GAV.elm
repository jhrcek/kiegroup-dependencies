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
    = GroupId String


type ArtifactId
    = ArtifactId String


type Version
    = Version String


groupIdParser : UrlParser.Parser (GroupId -> a) a
groupIdParser =
    UrlParser.custom "GROUP_ID" (Ok << GroupId)


artifactIdParser : UrlParser.Parser (ArtifactId -> a) a
artifactIdParser =
    UrlParser.custom "ARTIFACT_ID" (Ok << ArtifactId)


versionParser : UrlParser.Parser (Version -> a) a
versionParser =
    UrlParser.custom "VERSION" (Ok << Version)


groupIdToString : GroupId -> String
groupIdToString (GroupId g) =
    g


artifactIdToString : ArtifactId -> String
artifactIdToString (ArtifactId a) =
    a


versionToString : Version -> String
versionToString (Version v) =
    v
