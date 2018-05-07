module View.DependencyGraph exposing (view)

import Data.Coordinate as Coord exposing (Coordinate)
import Graph exposing (Node, NodeId)
import Html exposing (Attribute, Html, a, text)
import Html.Attributes exposing (href, style)
import Html.Events exposing (onClick)
import Table exposing (defaultCustomizations)


type alias CoordinateInfo =
    Node Coordinate


view : (Table.State -> msg) -> (NodeId -> msg) -> Table.State -> List CoordinateInfo -> Html msg
view tableMsg detailsClickedMsg tableState coordinates =
    Table.view (tableConfig tableMsg detailsClickedMsg) tableState coordinates


tableConfig : (Table.State -> msg) -> (NodeId -> msg) -> Table.Config CoordinateInfo msg
tableConfig sortMsg detailsClickedMsg =
    Table.customConfig
        { toId = \ci -> Coord.toString ci.label
        , toMsg = sortMsg
        , columns =
            [ Table.stringColumn "GroupId" (.label >> .groupId)
            , Table.stringColumn "ArtifactId" (.label >> .artifactId)
            , Table.stringColumn "Packaging" (.label >> .packaging)
            , Table.stringColumn "Qualifier" (\ci -> Maybe.withDefault "-" ci.label.qualifier)
            , Table.stringColumn "Version" (.label >> .version)
            , detailsLinkColumn detailsClickedMsg
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


detailsLinkColumn : (NodeId -> msg) -> Table.Column CoordinateInfo msg
detailsLinkColumn detailsClickedMsg =
    let
        viewData : CoordinateInfo -> Table.HtmlDetails msg
        viewData n =
            { attributes = []
            , children = [ a [ href "#", onClick (detailsClickedMsg n.id) ] [ text "details" ] ]
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
