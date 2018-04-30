module View.DependencyGraph exposing (view)

import Data.Coordinate exposing (Coordinate)
import Html exposing (Html)
import Table


view : (Table.State -> msg) -> Table.State -> List Coordinate -> Html msg
view tableMsg tableState coordinates =
    Table.view (tableConfig tableMsg) tableState coordinates


tableConfig : (Table.State -> msg) -> Table.Config Coordinate msg
tableConfig tableMsg =
    Table.config
        { toId = \coord -> coord.groupId ++ coord.artifactId ++ coord.version
        , toMsg = tableMsg
        , columns =
            [ Table.stringColumn "GroupId" .groupId
            , Table.stringColumn "ArtifactId" .artifactId
            , Table.stringColumn "Packaging" .packaging
            , Table.stringColumn "Qualifier" (\c -> Maybe.withDefault "-" c.qualifier)
            , Table.stringColumn "Version" .version
            ]
        }
