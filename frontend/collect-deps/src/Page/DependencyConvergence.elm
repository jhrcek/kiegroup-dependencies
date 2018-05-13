module Page.DependencyConvergence exposing (view)

import Data.DependencyGraph exposing (DependencyGraph)
import Dict
import Dict.Extra
import Graph
import Html exposing (Html, caption, div, h1, table, td, text, th, tr)
import Page exposing (Page(..))
import Set


view : DependencyGraph -> Html a
view graph =
    let
        groupAndArtifact_to_versions =
            Graph.nodes graph
                |> List.map .label
                |> Dict.Extra.groupBy (\{ groupId, artifactId } -> ( groupId, artifactId ))
                |> Dict.map (\_ coordinates -> List.map .version coordinates |> Set.fromList |> Set.toList)
                |> Dict.filter (\_ versions -> List.length versions > 1)
                |> Dict.toList

        tableCaption =
            caption [] [ text "Artifacts that are present in multiple versions" ]

        tableHeading =
            tr []
                [ th [] [ text "GroupId" ]
                , th [] [ text "ArtifactId" ]
                , th [] [ text "Versions" ]
                ]

        tableRows =
            List.map
                (\( ( groupId, artifactId ), versions ) ->
                    tr []
                        [ td [] [ text artifactId ]
                        , td [] [ Page.groupArtifactLink groupId artifactId ]
                        , td [] [ text (String.join ", " versions) ]
                        ]
                )
                groupAndArtifact_to_versions
    in
    div []
        [ h1 [] [ text "Dependency convergence" ]
        , table [] (tableCaption :: tableHeading :: tableRows)
        ]
