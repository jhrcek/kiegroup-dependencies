module View.DependencyGraph exposing (dependencyTreeView, view)

import Data.Coordinate as Coord
import Data.DependencyGraph exposing (DependencyContext, DependencyGraph, DependencyNode)
import Data.Scope as Scope exposing (Scope(..))
import Data.Tree.Drawer exposing (drawHtml)
import Graph.Tree as Tree
import Html exposing (Attribute, Html, a, span, text)
import Html.Attributes exposing (class, classList, href)
import Page
import Table exposing (defaultCustomizations)


dependencyTreeView : Tree.Tree DependencyContext -> DependencyGraph -> Html msg
dependencyTreeView tree graph =
    let
        contextDrawer : ( DependencyNode, Maybe Scope ) -> Html msg
        contextDrawer ( node, mScope ) =
            let
                coordinateLink =
                    a
                        [ href (Page.toUrlHash (Page.CoordinateDetails node.id))
                        , highlightCoordinate node.label.isOur
                        ]
                        [ text (Coord.toString node.label) ]

                scopeInfo =
                    mScope
                        |> Maybe.map
                            (\scope ->
                                [ span [ class (Scope.toCssClass scope) ]
                                    [ text (Scope.toString scope) ]
                                ]
                            )
                        |> Maybe.withDefault []
            in
            span [] (coordinateLink :: scopeInfo)
    in
    drawHtml contextDrawer <| Data.DependencyGraph.convertTree graph Nothing tree


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
    { defaultCustomizations | rowAttrs = \node -> [ highlightCoordinate node.label.isOur ] }


highlightCoordinate : Bool -> Attribute msg
highlightCoordinate isHighlighted =
    classList [ ( "highlight", isHighlighted ) ]
