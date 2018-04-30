module Page.Home exposing (view)

import Data.Github as Github
import Html exposing (Html, a, div, text)
import Route


view : List Github.RepoId -> Html a
view repoIds =
    div [] <| List.map repoLink repoIds


repoLink : Github.RepoId -> Html a
repoLink repoId =
    div []
        [ a [ Route.href (Route.Modules repoId) ]
            [ text <| Github.repoToString repoId ]
        ]
