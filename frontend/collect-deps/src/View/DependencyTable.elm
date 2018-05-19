module View.DependencyTable exposing (view)

import Data.Coordinate as Coord
import Data.DependencyGraph exposing (DependencyNode)
import Html exposing (Html, a, text)
import Html.Attributes exposing (href)
import Page
import Table exposing (defaultCustomizations)


view : (Table.State -> msg) -> Table.State -> List DependencyNode -> Html msg
view tableMsg tableState coordinates =
    Table.view (tableConfig tableMsg) tableState coordinates


tableConfig : (Table.State -> msg) -> Table.Config DependencyNode msg
tableConfig sortMsg =
    Table.customConfig
        { toId = \n -> toString n.id
        , toMsg = sortMsg
        , columns =
            [ groupIdLinkColumn
            , artifactIdLinkColumn
            , versionLinkColumn
            , Table.stringColumn "Packaging" (.label >> .packaging)
            , Table.stringColumn "Qualifier" (\n -> Maybe.withDefault "-" n.label.qualifier)
            , Table.intColumn "RTDs" (.label >> .reverseTransitiveDepsCount)
            , Table.intColumn "RDs" (.label >> .reverseDirectDepsCount)
            , Table.intColumn "DDs" (.label >> .directDepsCount)
            , Table.intColumn "TDs" (.label >> .transitiveDepsCount)
            , detailsLinkColumn
            ]
        , customizations = highlightOurCoordinates
        }


groupIdLinkColumn : Table.Column DependencyNode msg
groupIdLinkColumn =
    let
        viewData : DependencyNode -> Table.HtmlDetails msg
        viewData n =
            { attributes = []
            , children = [ Page.groupLink n.label.groupId ]
            }
    in
    Table.veryCustomColumn
        { name = "GroupId"
        , viewData = viewData
        , sorter = Table.increasingOrDecreasingBy (.label >> .groupId)
        }


artifactIdLinkColumn : Table.Column DependencyNode msg
artifactIdLinkColumn =
    let
        viewData : DependencyNode -> Table.HtmlDetails msg
        viewData n =
            { attributes = []
            , children = [ Page.groupArtifactLink n.label.groupId n.label.artifactId ]
            }
    in
    Table.veryCustomColumn
        { name = "ArtifactId"
        , viewData = viewData
        , sorter = Table.increasingOrDecreasingBy (.label >> .artifactId)
        }


versionLinkColumn : Table.Column DependencyNode msg
versionLinkColumn =
    let
        viewData : DependencyNode -> Table.HtmlDetails msg
        viewData n =
            { attributes = []
            , children = [ Page.groupArtifactVersionLink n.label.groupId n.label.artifactId n.label.version ]
            }
    in
    Table.veryCustomColumn
        { name = "Version"
        , viewData = viewData
        , sorter = Table.increasingOrDecreasingBy (.label >> .version)
        }


detailsLinkColumn : Table.Column DependencyNode msg
detailsLinkColumn =
    let
        viewData : DependencyNode -> Table.HtmlDetails msg
        viewData n =
            { attributes = []
            , children = [ a [ href (Page.toUrlHash (Page.CoordinateDetails n.id)) ] [ text "details" ] ]
            }
    in
    Table.veryCustomColumn
        { name = "Details"
        , viewData = viewData
        , sorter = Table.unsortable
        }


highlightOurCoordinates : Table.Customizations DependencyNode msg
highlightOurCoordinates =
    { defaultCustomizations | rowAttrs = \node -> [ Coord.highlight node.label.isOur ] }
