module Page
    exposing
        ( Page(CoordinateDetails, DependencyConvergence, Group, GroupArtifact, GroupArtifactVersion, Home)
        , groupArtifactLink
        , groupArtifactVersionLink
        , groupLink
        , homeLink
        , parseLocation
        , toBreadCrumb
        , toUrlHash
        )

import Data.Coordinate as Coord
import Data.DependencyGraph exposing (DependencyGraph)
import Graph exposing (NodeId)
import Html exposing (Html, a, div, text)
import Html.Attributes exposing (href)
import Navigation
import UrlParser as P exposing ((</>), Parser, int, map, oneOf, s, string, top)


type Page
    = Home
    | Group String
    | GroupArtifact String String
    | GroupArtifactVersion String String String
    | CoordinateDetails NodeId
    | DependencyConvergence


toBreadCrumb : Page -> DependencyGraph -> Html a
toBreadCrumb page graph =
    let
        joinWithArrow items =
            List.intersperse (text " » ") items |> div []
    in
    case page of
        Home ->
            joinWithArrow [ text "Home" ]

        DependencyConvergence ->
            joinWithArrow
                [ homeLink
                , text "Dependency Convergence"
                ]

        Group groupId ->
            joinWithArrow
                [ homeLink
                , text groupId
                ]

        GroupArtifact groupId artifactId ->
            joinWithArrow
                [ homeLink
                , groupLink groupId
                , text artifactId
                ]

        GroupArtifactVersion groupId artifactId version ->
            joinWithArrow
                [ homeLink
                , groupLink groupId
                , groupArtifactLink groupId artifactId
                , text version
                ]

        CoordinateDetails nodeId ->
            case Graph.get nodeId graph of
                Nothing ->
                    homeLink

                Just ctx ->
                    let
                        { groupId, artifactId, version } =
                            ctx.node.label
                    in
                    joinWithArrow
                        [ homeLink
                        , groupLink groupId
                        , groupArtifactLink groupId artifactId
                        , groupArtifactVersionLink groupId artifactId version
                        , text (Coord.toString ctx.node.label)
                        ]


homeLink : Html msg
homeLink =
    a [ href <| toUrlHash Home ] [ text "Home" ]


groupLink : String -> Html msg
groupLink groupId =
    a [ href <| toUrlHash <| Group groupId ] [ text groupId ]


groupArtifactLink : String -> String -> Html msg
groupArtifactLink groupId artifactId =
    a [ href <| toUrlHash <| GroupArtifact groupId artifactId ] [ text artifactId ]


groupArtifactVersionLink : String -> String -> String -> Html msg
groupArtifactVersionLink groupId artifactId version =
    a [ href <| toUrlHash <| GroupArtifactVersion groupId artifactId version ] [ text version ]


toUrlHash : Page -> String
toUrlHash page =
    let
        pieces =
            case page of
                Home ->
                    []

                DependencyConvergence ->
                    [ "dependency-convergence" ]

                Group groupId ->
                    [ "group", groupId ]

                GroupArtifact groupId artifactId ->
                    [ "group", groupId, "artifact", artifactId ]

                GroupArtifactVersion groupId artifactId version ->
                    [ "group", groupId, "artifact", artifactId, "version", version ]

                CoordinateDetails nodeId ->
                    [ "coordinate", toString nodeId ]
    in
    String.join "/" ("#" :: pieces)


route : Parser (Page -> a) a
route =
    oneOf
        [ map Home top
        , map Group (s "group" </> string)
        , map DependencyConvergence (s "dependency-convergence")
        , map GroupArtifact (s "group" </> string </> s "artifact" </> string)
        , map GroupArtifactVersion (s "group" </> string </> s "artifact" </> string </> s "version" </> string)
        , map CoordinateDetails (s "coordinate" </> int)
        ]


parseLocation : Navigation.Location -> Page
parseLocation location =
    P.parseHash route location |> Maybe.withDefault Home
