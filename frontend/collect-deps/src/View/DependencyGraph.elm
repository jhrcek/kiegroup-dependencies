module View.DependencyGraph exposing (ourArtifactStyle, view)

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
        { toId = \n -> Coord.toString n.label
        , toMsg = sortMsg
        , columns =
            [ Table.stringColumn "3rd Party"
                (\n ->
                    if n.label.is3rdParty then
                        "Y"
                    else
                        "N"
                )
            , Table.stringColumn "GroupId" (.label >> .groupId)
            , Table.stringColumn "ArtifactId" (.label >> .artifactId)
            , Table.stringColumn "Packaging" (.label >> .packaging)
            , Table.stringColumn "Qualifier" (\n -> Maybe.withDefault "-" n.label.qualifier)
            , Table.stringColumn "Version" (.label >> .version)
            , detailsLinkColumn detailsClickedMsg
            ]
        , customizations = highlightOurCoordinates
        }


highlightOurCoordinates : Table.Customizations CoordinateInfo msg
highlightOurCoordinates =
    let
        toRowAttrs n =
            if n.label.is3rdParty then
                []
            else
                [ ourArtifactStyle ]
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


ourArtifactStyle : Attribute msg
ourArtifactStyle =
    style [ ( "background-color", "lightblue" ) ]
