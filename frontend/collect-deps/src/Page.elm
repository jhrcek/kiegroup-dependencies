module Page
    exposing
        ( Page(CoordinateDetails, Group, GroupArtifact, GroupArtifactVersion, Home)
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


toBreadCrumb : Page -> DependencyGraph -> Html a
toBreadCrumb page graph =
    case page of
        Home ->
            div [] [ text "Home" ]

        Group groupId ->
            div [] [ homeLink, text (" » " ++ groupId) ]

        GroupArtifact groupId artifactId ->
            div [] [ homeLink, text " » ", groupLink groupId, text (" » " ++ artifactId) ]

        GroupArtifactVersion groupId artifactId version ->
            div [] [ homeLink, text " » ", groupLink groupId, text " » ", groupArtifactLink groupId artifactId, text (" » " ++ version) ]

        CoordinateDetails nodeId ->
            case Graph.get nodeId graph of
                Nothing ->
                    homeLink

                Just ctx ->
                    let
                        { groupId, artifactId, version } =
                            ctx.node.label
                    in
                    div []
                        [ homeLink
                        , text " » "
                        , groupLink groupId
                        , text " » "
                        , groupArtifactLink groupId artifactId
                        , text " » "
                        , groupArtifactVersionLink groupId artifactId version
                        , text (" » " ++ Coord.toString ctx.node.label)
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
        , map GroupArtifact (s "group" </> string </> s "artifact" </> string)
        , map GroupArtifactVersion (s "group" </> string </> s "artifact" </> string </> s "version" </> string)
        , map CoordinateDetails (s "coordinate" </> int)
        ]


parseLocation : Navigation.Location -> Page
parseLocation location =
    P.parseHash route location |> Maybe.withDefault Home
