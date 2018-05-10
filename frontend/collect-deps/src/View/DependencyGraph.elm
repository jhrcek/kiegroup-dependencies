module View.DependencyGraph exposing (dependencyTreeView, view)

import Data.Coordinate as Coord
import Data.DependencyGraph exposing (DependencyContext, DependencyNode)
import Data.Tree.Drawer exposing (drawHtml)
import Graph.Tree as Tree
import Html exposing (Attribute, Html, a, text)
import Html.Attributes exposing (href, style)
import Page
import Table exposing (defaultCustomizations)


dependencyTreeView : Tree.Tree DependencyContext -> Html msg
dependencyTreeView tree =
    let
        contextDrawer : DependencyContext -> Html msg
        contextDrawer dependencyContext =
            let
                styl =
                    if dependencyContext.node.label.isOur then
                        [ outCoordinateStyle ]
                    else
                        []
            in
            a (href (Page.toUrlHash (Page.CoordinateDetails dependencyContext.node.id)) :: styl)
                [ text <| Coord.toString dependencyContext.node.label ]
    in
    drawHtml contextDrawer <| Data.DependencyGraph.convertTree tree


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
    let
        toRowAttrs ci =
            if ci.label.isOur then
                [ outCoordinateStyle ]
            else
                []
    in
    { defaultCustomizations | rowAttrs = toRowAttrs }


outCoordinateStyle : Attribute msg
outCoordinateStyle =
    style [ ( "background-color", "lightblue" ) ]
