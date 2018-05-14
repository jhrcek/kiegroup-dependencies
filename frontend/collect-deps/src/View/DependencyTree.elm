module View.DependencyTree exposing (view)

import Data.Coordinate as Coord
import Data.DependencyGraph exposing (DependencyNode)
import Data.DependencyTree exposing (Tree(Node))
import Data.Scope as Scope exposing (Scope(..))
import Html exposing (Html, a, span, text)
import Html.Attributes exposing (class, href, style)
import Page


view : Tree ( DependencyNode, Maybe Scope ) -> Html msg
view =
    drawTree nodeDrawer


nodeDrawer : ( DependencyNode, Maybe Scope ) -> Html msg
nodeDrawer ( node, mScope ) =
    let
        coordinateLink =
            a
                [ href (Page.toUrlHash (Page.CoordinateDetails node.id))
                , Coord.highlight node.label.isOur
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


drawTree : (a -> Html msg) -> Tree a -> Html msg
drawTree nodeDrawer tree =
    drawTreeHelp nodeDrawer tree
        |> List.intersperse (T "\n")
        |> mergeConsecutiveTexts
        |> Html.div [ style [ ( "white-space", "pre" ), ( "font-family", "monospace" ) ] ]


drawTreeHelp : (a -> Html msg) -> Tree a -> List (Thing msg)
drawTreeHelp nodeDrawer (Node root children) =
    let
        drawSubTrees : List (Tree a) -> List (Thing msg)
        drawSubTrees ts =
            case ts of
                [] ->
                    []

                [ t ] ->
                    T "│" :: shift "└─ " "   " (drawTreeHelp nodeDrawer t)

                t :: ts1 ->
                    T "│" :: shift "├─ " "│  " (drawTreeHelp nodeDrawer t) ++ drawSubTrees ts1

        shift : String -> String -> List (Thing msg) -> List (Thing msg)
        shift first other =
            List.map2 addPrefix (first :: List.repeat 10000 other)
    in
    C "" (nodeDrawer root) :: drawSubTrees children


mergeConsecutiveTexts : List (Thing a) -> List (Html a)
mergeConsecutiveTexts xs =
    case xs of
        [] ->
            []

        [ T x ] ->
            [ Html.text x ]

        [ C x html ] ->
            [ Html.text x, html ]

        (T x) :: (T y) :: rest ->
            mergeConsecutiveTexts (T (x ++ y) :: rest)

        (T x) :: (C y html) :: rest ->
            Html.text (x ++ y) :: html :: mergeConsecutiveTexts rest

        (C x html) :: rest ->
            Html.text x :: html :: mergeConsecutiveTexts rest


type Thing a
    = T String -- text-only node whose test needs to be merge into following node
    | C String (Html a) -- arbitrary html element preceded by some text


addPrefix : String -> Thing a -> Thing a
addPrefix prefix thing =
    case thing of
        T x ->
            T (prefix ++ x)

        C x html ->
            C (prefix ++ x) html
