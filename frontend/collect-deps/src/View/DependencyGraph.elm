module View.DependencyGraph exposing (dependencyTreeView, view)

import Data.Coordinate as Coord exposing (Coordinate)
import Data.DependencyGraph exposing (DependencyContext)
import Data.Tree.Drawer exposing (drawHtml)
import Graph exposing (Node)
import Graph.Tree as Tree
import Html exposing (Attribute, Html, a, text)
import Html.Attributes exposing (href, style)
import Page exposing (Page(CoordinateDetails))
import Table exposing (defaultCustomizations)


type alias CoordinateInfo =
    Node Coordinate


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
            a (href (Page.toUrlHash (CoordinateDetails dependencyContext.node.id)) :: styl)
                [ text <| Coord.toString dependencyContext.node.label ]
    in
    drawHtml contextDrawer <| Data.DependencyGraph.convertTree tree


view : (Table.State -> msg) -> Table.State -> List CoordinateInfo -> Html msg
view tableMsg tableState coordinates =
    Table.view (tableConfig tableMsg) tableState coordinates


tableConfig : (Table.State -> msg) -> Table.Config CoordinateInfo msg
tableConfig sortMsg =
    Table.customConfig
        { toId = \ci -> Coord.toString ci.label
        , toMsg = sortMsg
        , columns =
            [ Table.stringColumn "GroupId" (.label >> .groupId)
            , Table.stringColumn "ArtifactId" (.label >> .artifactId)
            , Table.stringColumn "Packaging" (.label >> .packaging)
            , Table.stringColumn "Qualifier" (\ci -> Maybe.withDefault "-" ci.label.qualifier)
            , Table.stringColumn "Version" (.label >> .version)
            , detailsLinkColumn
            ]
        , customizations = highlightOurCoordinates
        }


highlightOurCoordinates : Table.Customizations CoordinateInfo msg
highlightOurCoordinates =
    let
        toRowAttrs ci =
            if ci.label.isOur then
                [ outCoordinateStyle ]
            else
                []
    in
    { defaultCustomizations | rowAttrs = toRowAttrs }


detailsLinkColumn : Table.Column CoordinateInfo msg
detailsLinkColumn =
    let
        viewData : CoordinateInfo -> Table.HtmlDetails msg
        viewData n =
            { attributes = []
            , children = [ a [ href (Page.toUrlHash (CoordinateDetails n.id)) ] [ text "details" ] ]
            }
    in
    Table.veryCustomColumn
        { name = "Details"
        , viewData = viewData
        , sorter = Table.unsortable
        }


outCoordinateStyle : Attribute msg
outCoordinateStyle =
    style [ ( "background-color", "lightblue" ) ]
