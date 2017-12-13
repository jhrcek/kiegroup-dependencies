module Route exposing (Route(..), fromLocation, href, modifyUrl)

import Data.GAV as GAV
import Data.Github as Github
import Data.Maven as Maven
import Html exposing (Attribute)
import Html.Attributes as Attr exposing (scope)
import Navigation exposing (Location)
import UrlParser as Url exposing ((</>), Parser, oneOf, parseHash, s, string)


-- ROUTING --


type Route
    = Home -- Summary
      -- From artifacts to maven modules from kiegroup projects
    | Groups -- list of all (kiegroup and 3rd party) groups
    | Artifacts GAV.GroupId -- list all artifactId from that group
    | Versions GAV.GroupId GAV.ArtifactId -- list all versions of that groupId + artifactId
    | KieModules GAV.GroupId GAV.ArtifactId GAV.Version -- list of kiegroup modules that have this GAV in dependency tree
    | ArtifactSearch -- filtering based on GAV
      -- From kiegroup repos to artifacts
    | Repos -- list of kiegroup repos that are part of kie-ci
    | Modules Github.RepoId -- tree of maven modules in given repo
    | DependencyTree Github.RepoId Maven.Coordinate -- Dependency tree of given maven module


route : Parser (Route -> a) a
route =
    oneOf
        [ Url.map Home (s "")
        , Url.map Groups (s "groups")
        , Url.map Artifacts (s "groups" </> GAV.groupIdParser </> s "artifacts")
        , Url.map Versions (s "groups" </> GAV.groupIdParser </> s "artifacts" </> GAV.artifactIdParser </> s "versions")
        , Url.map KieModules (s "groups" </> GAV.groupIdParser </> s "artifacts" </> GAV.artifactIdParser </> s "versions" </> GAV.versionParser </> s "kiemodules")
        , Url.map Repos (s "repositories")
        , Url.map Modules (s "repositories" </> Github.repoParser </> s "modules")
        , Url.map DependencyTree (s "repositories" </> Github.repoParser </> s "modules" </> Maven.coordinateParser </> s "dependencies")
        ]



-- INTERNAL --


routeToString : Route -> String
routeToString page =
    let
        pieces =
            case page of
                Home ->
                    []

                Groups ->
                    [ "groups" ]

                Artifacts groupId ->
                    [ "groups", GAV.groupIdToString groupId, "artifacts" ]

                Versions groupId artifactId ->
                    [ "groups", GAV.groupIdToString groupId, "artifacts", GAV.artifactIdToString artifactId, "versions" ]

                KieModules groupId artifactId version ->
                    [ "groups", GAV.groupIdToString groupId, "artifacts", GAV.artifactIdToString artifactId, "versions", GAV.versionToString version, "kiemodules" ]

                ArtifactSearch ->
                    [ "search" ]

                Repos ->
                    [ "repositories" ]

                Modules repoId ->
                    [ "repositories", Github.repoToString repoId, "modules" ]

                DependencyTree repoId kieCoord ->
                    [ "repositories", Github.repoToString repoId, "modules", Maven.coordinateToString kieCoord, "dependencies" ]
    in
    "#/" ++ String.join "/" pieces



-- PUBLIC HELPERS --


href : Route -> Attribute msg
href route =
    Attr.href (routeToString route)


modifyUrl : Route -> Cmd msg
modifyUrl =
    routeToString >> Navigation.modifyUrl


fromLocation : Location -> Maybe Route
fromLocation location =
    if String.isEmpty location.hash then
        Just Home
    else
        parseHash route location
