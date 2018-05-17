module Page
    exposing
        ( Page(AllArtifacts, CoordinateDetails, DependencyConvergence, Group, GroupArtifact, GroupArtifactVersion, Help)
        , groupArtifactLink
        , groupArtifactVersionLink
        , groupLink
        , navigation
        , parseLocation
        , toUrlHash
        )

import Data.Coordinate as Coord
import Data.DependencyGraph exposing (DependencyGraph)
import Graph exposing (NodeId)
import Html exposing (Html, a, div, text)
import Html.Attributes exposing (class, classList, href)
import Navigation
import UrlParser as P exposing ((</>), Parser, int, map, oneOf, s, string, top)


type Page
    = Help
    | AllArtifacts
    | Group String
    | GroupArtifact String String
    | GroupArtifactVersion String String String
    | CoordinateDetails NodeId
    | DependencyConvergence


type NavItem
    = ArtifactsNav
    | DepConvNav
    | HelpNav


navBar : Page -> Html a
navBar currentPage =
    div [ class "topnav" ]
        [ artifactsNavLink currentPage
        , dependencyConvergenceNavLink currentPage
        , helpNavLink currentPage
        ]


navigation : Page -> DependencyGraph -> Html a
navigation currentPage graph =
    div []
        [ navBar currentPage
        , breadcrumb currentPage graph
        ]


toNavItem : Page -> NavItem
toNavItem page =
    case page of
        Help ->
            HelpNav

        DependencyConvergence ->
            DepConvNav

        AllArtifacts ->
            ArtifactsNav

        Group _ ->
            ArtifactsNav

        GroupArtifact _ _ ->
            ArtifactsNav

        GroupArtifactVersion _ _ _ ->
            ArtifactsNav

        CoordinateDetails _ ->
            ArtifactsNav


breadcrumb : Page -> DependencyGraph -> Html a
breadcrumb page graph =
    let
        joinWithArrow items =
            List.intersperse (text " » ") items |> div [ class "breadcrumb" ]
    in
    case page of
        Help ->
            text ""

        DependencyConvergence ->
            text ""

        AllArtifacts ->
            joinWithArrow
                [ text "All Artifacts" ]

        Group groupId ->
            joinWithArrow
                [ allArtifactsLink
                , text groupId
                ]

        GroupArtifact groupId artifactId ->
            joinWithArrow
                [ allArtifactsLink
                , groupLink groupId
                , text artifactId
                ]

        GroupArtifactVersion groupId artifactId version ->
            joinWithArrow
                [ allArtifactsLink
                , groupLink groupId
                , groupArtifactLink groupId artifactId
                , text version
                ]

        CoordinateDetails nodeId ->
            case Graph.get nodeId graph of
                Nothing ->
                    text ""

                Just ctx ->
                    let
                        { groupId, artifactId, version } =
                            ctx.node.label
                    in
                    joinWithArrow
                        [ allArtifactsLink
                        , groupLink groupId
                        , groupArtifactLink groupId artifactId
                        , groupArtifactVersionLink groupId artifactId version
                        , text (Coord.toString ctx.node.label)
                        ]


allArtifactsLink : Html msg
allArtifactsLink =
    a [ href <| toUrlHash AllArtifacts ] [ text "All Artifacts" ]


artifactsNavLink : Page -> Html msg
artifactsNavLink =
    navLink "Artifacts" AllArtifacts ArtifactsNav


dependencyConvergenceNavLink : Page -> Html msg
dependencyConvergenceNavLink =
    navLink "Dependency Convergence" DependencyConvergence DepConvNav


helpNavLink : Page -> Html msg
helpNavLink =
    navLink "Help" Help HelpNav


navLink : String -> Page -> NavItem -> Page -> Html msg
navLink linkText targetPage navItem currentPage =
    a [ href <| toUrlHash targetPage, classList [ ( "active", toNavItem currentPage == navItem ) ] ]
        [ text linkText ]


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
                AllArtifacts ->
                    []

                Help ->
                    [ "help" ]

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
        [ map AllArtifacts top
        , map Help (s "help")
        , map Group (s "group" </> string)
        , map DependencyConvergence (s "dependency-convergence")
        , map GroupArtifact (s "group" </> string </> s "artifact" </> string)
        , map GroupArtifactVersion (s "group" </> string </> s "artifact" </> string </> s "version" </> string)
        , map CoordinateDetails (s "coordinate" </> int)
        ]


parseLocation : Navigation.Location -> Page
parseLocation location =
    P.parseHash route location |> Maybe.withDefault AllArtifacts
