module View.DependencyGraph exposing (view)

import Data.Coordinate as Coord exposing (Coordinate)
import Graph exposing (Node, NodeId)
import Html exposing (Html, a, text)
import Html.Attributes exposing (href)
import Html.Events exposing (onClick)
import Table


type alias CoordinateInfo =
    Node Coordinate


view : (Table.State -> msg) -> (NodeId -> msg) -> Table.State -> List CoordinateInfo -> Html msg
view tableMsg detailsClickedMsg tableState coordinates =
    Table.view (tableConfig tableMsg detailsClickedMsg) tableState coordinates


tableConfig : (Table.State -> msg) -> (NodeId -> msg) -> Table.Config CoordinateInfo msg
tableConfig sortMsg detailsClickedMsg =
    Table.config
        { toId = \n -> Coord.toString n.label
        , toMsg = sortMsg
        , columns =
            [ Table.stringColumn "GroupId" (.label >> .groupId)
            , Table.stringColumn "ArtifactId" (.label >> .artifactId)
            , Table.stringColumn "Packaging" (.label >> .packaging)
            , Table.stringColumn "Qualifier" (\n -> Maybe.withDefault "-" n.label.qualifier)
            , Table.stringColumn "Version" (.label >> .version)
            , detailsLinkColumn detailsClickedMsg
            ]
        }


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
