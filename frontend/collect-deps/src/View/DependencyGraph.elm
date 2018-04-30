module View.DependencyGraph exposing (view)

import Data.DependencyGraph exposing (DependencyGraph)
import Html exposing (Html, div, text)


view : DependencyGraph -> Html a
view dependencyGraph =
    div [] [ text "todo" ]
