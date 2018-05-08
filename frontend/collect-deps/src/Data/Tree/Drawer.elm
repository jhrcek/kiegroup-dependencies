module Data.Tree.Drawer exposing (Tree(Node), drawHtml)

import Html exposing (Html)
import Html.Attributes exposing (style)


type Tree a
    = Node a (List (Tree a))


drawHtml : (a -> Html msg) -> Tree a -> Html msg
drawHtml nodeDrawer tree =
    drawHtmlHelp nodeDrawer tree
        |> List.intersperse (T "\n")
        |> mergeConsecutiveTexts
        |> Html.div [ style [ ( "white-space", "pre" ), ( "font-family", "monospace" ) ] ]


drawHtmlHelp : (a -> Html msg) -> Tree a -> List (Thing msg)
drawHtmlHelp nodeDrawer (Node root children) =
    let
        drawSubTrees : List (Tree a) -> List (Thing msg)
        drawSubTrees ts =
            case ts of
                [] ->
                    []

                [ t ] ->
                    T "│" :: shift "└─ " "   " (drawHtmlHelp nodeDrawer t)

                t :: ts1 ->
                    T "│" :: shift "├─ " "│  " (drawHtmlHelp nodeDrawer t) ++ drawSubTrees ts1

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
    = T String -- text-only node which needs to be merge with other text only nodes
    | C String (Html a) -- arbitrary html element preceded by some text


addPrefix : String -> Thing a -> Thing a
addPrefix prefix thing =
    case thing of
        T x ->
            T (prefix ++ x)

        C x html ->
            C (prefix ++ x) html
